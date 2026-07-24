package mycgra

import scala.collection.mutable.{ArrayBuffer, Queue, Map => MutableMap, Set => MutableSet}
import scala.io.Source
import java.io.FileNotFoundException
import scala.util.Random

// sbt "runMain mycgra.Main" > output.log

class CgraSimulator(val rows: Int = 8, val cols: Int = 8) {
  val if_print: Boolean = true
  // --- Configuration and Data Structures ---

  /** The width of the data and instructions in bits. */
  val DATA_WIDTH: Int = 32
  val INSTRUCTION_WIDTH: Int = 64
  val IN_FIFO_DEPTH: Int = 4

  /** Represents a single PE's state. */
  sealed trait PEState
  case object Stall extends PEState
  case object CanRun extends PEState

  /** Represents the different types of instructions. */
  //instruction type, 0 for add, 1 for sub, 2 for eq, 3 for lt, 4 for gt, 5 for and, 6 for or, 7 for not, 8 for left shift
  //9 for unsigend right shift, 10 for signed right shift, 11 for xor, 12 for mul, 13 for udiv, 14 for sdiv, 15 for urem, 16 for srem,
  //17 for gen, 18 for loopoutData, 19 for slt, 20 for sgt, 21 for fmul, 22 for fadd, 23 for fdiv, 24 for fsub, 25 for fp2int, 26 for int2fp
  //27 for load, 28 for store, 29 for phibranchData, 30 for philoopData, 31 for fornormal, 32 for forstep, 33 for forupper, 34 for forall
  //35 for phibranchPred, 36 for philoopPred, 37 for loopoutPred, 38 for uneq, 39 for leq, 40 for geq, 41 for sleq, 42 for sgeq, 43 for route, 44 for mergepred

  object InstructionType extends Enumeration {
    type InstructionType = Value
    val ADD, SUB, EQ, LT, GT, AND, OR, NOT, LSHIFT, URSHIFT, SRSHIFT, XOR,
    MUL, UDIV, SDIV, UREM, SREM, GEN, LOOP_OUT_DATA, SLT, SGT, FMUL, FADD, FDIV, FSUB,
    FP2INT, INT2FP, LOAD, STORE, PHI_BRANCH_DATA, PHI_LOOP_DATA, FOR_NORMAL, FOR_STEP,
    FOR_UPPER, FOR_ALL, PHI_BRANCH_PRED, PHI_LOOP_PRED, LOOP_OUT_PRED, UNEQ, LEQ, GEQ, SLEQ, SGEQ, ROUTE, MERGE_PRED, NULL = Value

    def fromInt(i: Int): Option[InstructionType] = values.find(_.id == i)
  }

  /**
   * Represents a parsed instruction.
   *
   * @param predInConfig       Configuration for predicate inputs (0-1 bits)
   * @param outConfig          Configuration for output (2-3 bits)
   * @param dataInConfig       Configuration for data inputs (4-6 bits)
   * @param opType             Instruction type (7-12 bits)
   * @param immPredConfig      Special immediate/predicate config (13-15 bits)
   * @param immediateValue     The immediate value if present
   * @param destPEID           The ID of the destination PE
   * @param destChannelID      The ID of the destination channel (0-3 for d0, d1, p0, p1)
   */
  case class Instruction(
    predInConfig: Int,
    outConfig: Int,
    dataInConfig: Int,
    var opType: InstructionType.Value,
    immPredConfig: Int,
    immediateValue: Int,
    destPEID: Int,
    destChannelID: Int
  )

  /**
   * Represents a single Processing Element.
   *
   * @param id                Unique ID of the PE
   * @param row               Row index in the PE array
   * @param col               Column index in the PE array
   * @param instruction       The parsed instruction this PE holds
   * @param state             The current state (CanRun or Stall)
   * @param inDataFifo0       FIFO for data input channel 0
   * @param inDataFifo1       FIFO for data input channel 1
   * @param inPredFifo0       FIFO for predicate input channel 0
   * @param inPredFifo1       FIFO for predicate input channel 1
   * @param tempOutBuffer     Temporary buffer for the output data
   * @param tempOutPredBuffer Temporary buffer for the output predicate
   * @param outValid          Signal indicating if the output buffer is valid
   */
  case class PE(
    id: Int,
    row: Int,
    col: Int,
    var instruction: Instruction,
    var state: PEState = CanRun,
    val inDataFifo0: Queue[Int] = Queue.empty,
    val inDataFifo1: Queue[Int] = Queue.empty,
    val inPredFifo0: Queue[Int] = Queue.empty,
    val inPredFifo1: Queue[Int] = Queue.empty,
    var tempOutBuffer: Int = 0,
    var tempOutPredBuffer: Int = 0,
    var outValid: Boolean = false,
    var fsms: Map[InstructionType.Value, StatefulInstruction] = Map.empty,
    var datain0_valid: Boolean = false,
    var datain1_valid: Boolean = false,
    var predin0_valid: Boolean = false,
    var predin1_valid: Boolean = false,
    var loop_out_valid: Boolean = false,
    var phi_loop_valid: Boolean = false,
    var phi_loop_imm_end: Boolean = false,
    var execute_once: Boolean = false,
    var route_data_valid: Boolean = false,
    var route_pred_valid: Boolean = false,
    // Memory load/store tracking
    // case class MemReq(var data: Int = 0, var valid: Boolean = false),
    var currentLoadReq: Option[LoadReq] = None, // none: no load req now, can begin a new load
    // currentLoadReq.isEmpty: no load req now
    // currentLoadReq.isDefined && !valid: waiting for the memory to give results
    var currentStoreReq: Option[StoreReq] = None, 
    var current_state: PhiLoopState.Value = PhiLoopState.Initial
  )

  /**
   * A class to hold the parsed configuration from the input file.
   *
   * @param peIdToInstArray Map of PE ID to its 64-bit instruction represented as a bit array.
   * @param peIdToOutSetD0  Map of PE ID to a bit array representing destinations for output data 0.
   * @param peIdToOutSetD1  Map of PE ID to a bit array representing destinations for output data 1.
   * @param peIdToOutSetP0  Map of PE ID to a bit array representing destinations for output predicate 0.
   * @param peIdToOutSetP1  Map of PE ID to a bit array representing destinations for output predicate 1.
   * @param finalPlacements Map of instruction index to its PE placement ID.
   */
  case class CgraConfiguration(
    peIdToInstArray: Map[Int, Array[Int]],
    peIdToOutSetD0: Map[Int, Seq[Int]],
    peIdToOutSetD1: Map[Int, Seq[Int]],
    peIdToOutSetP0: Map[Int, Seq[Int]],
    peIdToOutSetP1: Map[Int, Seq[Int]],
    finalPlacements: Map[Int, Int]
  )

  import scala.collection.mutable.{Queue, ArrayBuffer}

  // Load/Store requests with remaining cycles
  case class LoadReq(addr: Int, var cyclesLeft: Int, var data: Int = 0, var valid: Boolean = false)
  case class StoreReq(addr: Int, data: Int, var cyclesLeft: Int, var valid: Boolean = false)

  import scala.util.Random

  class Memory(
      val size: Int = 65536,
      val readPorts: Int = 1,
      val writePorts: Int = 1,
      val accessLatency: Int = 2
  ) {
    // private 
    val mem = Array.fill(size)(0)
    

    private val loadQueue = Queue[LoadReq]()
    private val storeQueue = Queue[StoreReq]()



    /** PE issues a load request (non-blocking) */
    def load(addr: Int): LoadReq = {
      require(addr >= 0 && addr < size)
      val req = LoadReq(addr, accessLatency)
      loadQueue.enqueue(req)
      req
    }

    /** PE issues a store request (non-blocking) */
    def store(addr: Int, data: Int): StoreReq = {
      require(addr >= 0 && addr < size)
      val req = StoreReq(addr, data, accessLatency)
      storeQueue.enqueue(req)
      req
    }

    /** Step memory by one cycle */
    def step(): Unit = {
      // --- Update all stores ---
      storeQueue.foreach(_.cyclesLeft -= 1)
      var storePortsUsed = 0
      while (storePortsUsed < writePorts && storeQueue.nonEmpty && storeQueue.front.cyclesLeft <= 0) {
        val req = storeQueue.dequeue()
        mem(req.addr) = req.data
        req.valid = true
        storePortsUsed += 1
      }

      // --- Update all loads ---
      loadQueue.foreach(_.cyclesLeft -= 1)
      var loadPortsUsed = 0
      while (loadPortsUsed < readPorts && loadQueue.nonEmpty && loadQueue.front.cyclesLeft <= 0) {
        val req = loadQueue.dequeue()
        req.data = mem(req.addr)
        req.valid = true
        loadPortsUsed += 1
      }
    }
  }


  // --- Core Simulation Logic ---

  /**
   * Generates a sorted, unique list of destination PE IDs for a given source PE.
   * This list defines the mapping from `outSet` array index to destination PE ID.
   *
   * @param sourceId The ID of the source PE.
   * @param rows     The number of rows in the CGRA.
   * @param cols     The number of columns in the CGRA.
   * @return A sorted list of destination PE IDs.
   */
  def getOrderedDestinations13(sourceId: Int, rows: Int, cols: Int): List[Int] = {
    val sourceRow = sourceId / cols
    val sourceCol = sourceId % cols

    val destinations = MutableSet.empty[Int]

    // Add self-loop and 8 neighbors (3x3 grid)
    for {
      rOffset <- -1 to 1
      cOffset <- -1 to 1
    } {
      val destRow = (sourceRow + rOffset + rows) % rows
      val destCol = (sourceCol + cOffset + cols) % cols
      val destId = destRow * cols + destCol
      destinations += destId
    }

    // Add 4 additional neighbors two steps away
    destinations += (sourceRow * cols + ((sourceCol + 2) % cols))
    destinations += (sourceRow * cols + ((sourceCol - 2 + cols) % cols))
    destinations += (((sourceRow + 2) % rows) * cols + sourceCol)
    destinations += (((sourceRow - 2 + rows) % rows) * cols + sourceCol)

    // Ensure sourceId is first, and rest are sorted excluding sourceId
    val sortedRest = destinations.filter(_ != sourceId).toList.sorted
    sourceId :: sortedRest
  }

  def getOrderedDestinations(sourceId: Int, rows: Int, cols: Int): List[Int] = {
    val total = rows * cols
    val destinations = (0 until total).toList
    val sortedRest = destinations.filter(_ != sourceId).toList.sorted
    sourceId :: sortedRest
  }




  /**
   * Parses a raw 64-bit instruction into a structured `Instruction` object.
   *
   * @param rawInst The raw 64-bit instruction.
   * @return The parsed `Instruction` object.
   */
  def parseInstructionFromLong(rawInst: Long): Instruction = {
    /////////////////////////////from 0 to datawidth-1, 
    //the last two bits (1-0) show whether has predicate input, 00 for no, 01 for predicate in0, 10 for both, 11 for reverse predate in0.
    //the (3-2) bit indicate whether has predicate out or data out, 00 for no out, 01 for only data, 10 for only predicate, 11 for both
    //(6-4) indicate data input, 000 for no, 001 for only data in0, 010 for both data, 011 for data in0 and imm, 100 for imm and data in0, 101 for imm and data in1, 110 for Imm only
    ///////////////////////////////////////////////
    
    ////////(12-7) indicate the instruction type, 0 for add, 1 for sub, 2 for eq, 3 for lt, 4 for gt, 5 for and, 6 for or, 7 for not, 8 for left shift
    ////////////// 9 for unsigend right shift, 10 for signed right shift, 11 for xor, 12 for mul, 13 for udiv, 14 for sdiv, 15 for urem, 16 for srem,
    ///////////////17 for gen, 18 for loopoutData, 19 for slt, 20 for sgt, 21 for fmul, 22 for fadd, 23 for fdiv, 24 for fsub, 25 for fp2int, 26 for int2fp
    //27 for load, 28 for store, 29 for phibranchData, 30 for philoopData, 31 for fornormal, 32 for forstep, 33 for forupper, 34 for forall
    //35 for phibranchPred, 36 for philoopPred, 37 for loopoutPred, 38 for uneq, 39 for leq, 40 for geq, 41 for sleq, 42 for sgeq, 43 for route, 44 for mergepred
    //(15-13) represents which pred comes from Imm,01 for pred0, 10 for pred1, 11 for special case, pred0 is a Imm and only produce once, 100 for special case, data0 is a Imm and only produce once
    
    /////////(instructionWidth-instructionWidth/2) is for imm

    // Bitmasks for each field
    val predInConfig      = (rawInst & 0x3).toInt                  // bits 1-0 
    val outConfig         = ((rawInst >> 2) & 0x3).toInt           // bits 3-2
    val dataInConfig      = ((rawInst >> 4) & 0x7).toInt           // bits 6-4
    val opTypeInt         = ((rawInst >> 7) & 0x3F).toInt          // bits 12-7
    val immPredConfig     = ((rawInst >> 13) & 0x7).toInt          // bits 15-13
    val immBitsRaw = (rawInst >>> 32) & 0xFFFFFFFF
    val reversedImm = reverseBits32(immBitsRaw)
    val immediateValue = immBitsRaw.toInt
    if (if_print) {
      print("imm:")
      println(immediateValue)
      println("imm: " + java.lang.Float.intBitsToFloat(immediateValue))
    }

    val destPEID          = ((rawInst >> 48) & 0xFF).toInt         // bits 55-48
    val destChannelID     = ((rawInst >> 56) & 0x3).toInt          // bits 57-56

    // print(opTypeInt)
    var opType = InstructionType.fromInt(opTypeInt).getOrElse(InstructionType.NULL)
    // print(opType)
    if (opTypeInt == 0 && predInConfig == 0 && dataInConfig == 0 && outConfig == 0) {
      // println("rawbit:",rawInst)
      opType = InstructionType.NULL
    }

    Instruction(predInConfig, outConfig, dataInConfig, opType, immPredConfig, immediateValue, destPEID, destChannelID)
  }

  def reverseBits32(x: Long): Long = {
    var in  = x
    var out = 0L
    for (i <- 0 until 32) {
      val bit = (in >> i) & 1L
      out |= (bit << (31 - i))
    }
    out
  }

  /**
   * Converts a bit array (Array[Int] of 0s and 1s) to a single 64-bit Long.
   *
   * @param bits The array of bits.
   * @return The 64-bit Long value.
   */
  def parseBitArrayToLong(bits: Array[Int]): Long = {
    var result: Long = 0L
    for (i <- bits.indices) {
      if (bits(i) == 1) {
        result |= (1L << i)  // LSB at index 0
      }
    }
    result
  }


  /**
   * Reads instructions and routing from the input file and prints the parsed configuration.
   *
   * This refactored version correctly uses `finalplacements` to map instruction indices
   * to PE IDs before populating the configuration maps. It also now correctly
   * decodes and prints the destination PE IDs from the outSet arrays.
   *
   * @param fileContent The content of the configuration file.
   * @param rows The number of rows in the CGRA.
   * @param cols The number of columns in the CGRA.
   * @return A `CgraConfiguration` object containing all parsed information.
   */
  def readConfigFromFile(fileContent: String, rows: Int, cols: Int): CgraConfiguration = {
    val lines = fileContent.split("\n").map(_.trim).filterNot(_.isEmpty)

    // Temporary maps to hold data keyed by instruction index
    val tempFinalInstArray = MutableMap.empty[Int, Array[Int]]
    val tempOutSetD0 = MutableMap.empty[Int, Array[Int]]
    val tempOutSetD1 = MutableMap.empty[Int, Array[Int]]
    val tempOutSetP0 = MutableMap.empty[Int, Array[Int]]
    val tempOutSetP1 = MutableMap.empty[Int, Array[Int]]
    val tempFinalPlacements = MutableMap.empty[Int, Int]

    val pattern = """(\w+)\((\d+)\):\s*(.*)""".r

    // First pass: Read all data into temporary maps, keyed by instruction index
    for (line <- lines) {
      line match {
        case pattern(arrayName, indexStr, valuesStr) =>
          try {
            val index = indexStr.toInt
            val values = valuesStr.split(",").map(_.trim.toInt)

            arrayName match {
              case "finalInstArray" => tempFinalInstArray(index) = values
              case "outSetD0" => tempOutSetD0(index) = values
              case "outSetD1" => tempOutSetD1(index) = values
              case "outSetP0" => tempOutSetP0(index) = values
              case "outSetP1" => tempOutSetP1(index) = values
              case "finalplacements" => tempFinalPlacements(index) = values.head
              case _ => // Do nothing
            }
          } catch {
            case e: NumberFormatException =>
              println(s"Warning: Could not parse numbers from line '$line'. Skipping.")
          }
        case _ => // Skip comments, empty lines, or unrecognized lines
      }
    }

    // Final maps to be returned, keyed by PE ID
    val peIdToInstArray = MutableMap.empty[Int, Array[Int]]
    val peIdToOutSetD0 = MutableMap.empty[Int, Array[Int]]
    val peIdToOutSetD1 = MutableMap.empty[Int, Array[Int]]
    val peIdToOutSetP0 = MutableMap.empty[Int, Array[Int]]
    val peIdToOutSetP1 = MutableMap.empty[Int, Array[Int]]

    val totalPEs = rows * cols
    for (peId <- 0 until totalPEs) {
      peIdToInstArray(peId) = Array.fill(64)(0)
    }
    for (peId <- 0 until totalPEs) {
      peIdToOutSetD0(peId) = Array.fill(13)(0)
      peIdToOutSetD1(peId) = Array.fill(13)(0)
      peIdToOutSetP0(peId) = Array.fill(13)(0)
      peIdToOutSetP1(peId) = Array.fill(13)(0)
    }
    // Second pass: Iterate over all instruction indices. If the instruction is
    // not a no-op (i.e., finalInstArray is not all zeros), then use its
    // corresponding placement to populate the PE maps.
    for (instIndex <- tempFinalInstArray.keys.toList.sorted) {
      val instArray = tempFinalInstArray.getOrElse(instIndex, Array.empty[Int])
      // Check if the instruction array is all zeros
      if (instArray.exists(_ != 0)) {
        val peId = tempFinalPlacements.getOrElse(instIndex, -1)
        if (peId != -1) {
          peIdToInstArray(peId) = instArray
          peIdToOutSetD0(peId) = tempOutSetD0.getOrElse(instIndex, Array.empty[Int])
          peIdToOutSetD1(peId) = tempOutSetD1.getOrElse(instIndex, Array.empty[Int])
          peIdToOutSetP0(peId) = tempOutSetP0.getOrElse(instIndex, Array.empty[Int])
          peIdToOutSetP1(peId) = tempOutSetP1.getOrElse(instIndex, Array.empty[Int])
        } else {
          if (if_print)
            println(s"Warning: Instruction $instIndex is non-zero but has no placement in finalplacements. Skipping.")
        }
      } else {
        // This is a no-op instruction, so we do nothing with its placement.
        if (if_print) println(s"Debug: Instruction $instIndex is a no-op (all zeros), skipping placement.")
      }
    }
   
    
    // Print the parsed configuration
    if (if_print) {
    println("\n--- Parsed CGRA Configuration (Debug Output) ---")
    println("\nRaw outSetD0 arrays parsed from file:")
      tempOutSetD0.toSeq.sortBy(_._1).foreach { case (instIndex, arr) =>
        println(s"  outSetD0($instIndex): [${arr.mkString(", ")}]")
      }
    println("\nRaw outSetD1 arrays parsed from file:")
      tempOutSetD1.toSeq.sortBy(_._1).foreach { case (instIndex, arr) =>
        println(s"  outSetD1($instIndex): [${arr.mkString(", ")}]")
      }
    println("\nRaw outSetP0 arrays parsed from file:")
      tempOutSetP0.toSeq.sortBy(_._1).foreach { case (instIndex, arr) =>
        println(s"  outSetP0($instIndex): [${arr.mkString(", ")}]")
      }
      println("\nFinal Instructions (finalInstArray), mapped to PE IDs:")
      if (peIdToInstArray.isEmpty) {
        println("  (No data found)")
      } else {
        peIdToInstArray.toSeq.sortBy(_._1).foreach { case (peId, instArray) =>
          val instString = instArray.mkString(", ")
          println(s"PE($peId): $instString")
        }
      }

      println("\nOutput Data 0 Routes (outSetD0), mapped from PE ID:")
    }

    

    val ifPrint = false 
    val peIdToDestinationsD0: Map[Int, Seq[Int]] =
      if (peIdToOutSetD0.isEmpty) {
        if (ifPrint) println("  (No data found)")
        Map.empty[Int, Seq[Int]]
      } else {
        peIdToOutSetD0.toSeq.sortBy(_._1).map { case (from, toArray) =>
          val destinationList = getOrderedDestinations(from, rows, cols)
          if (ifPrint) println(s"  PE($from) destination list: $destinationList")

          val destinations = toArray.toSeq.zipWithIndex 
            .filter(_._1 == 1)
            .map { case (_, index) => destinationList.lift(index).getOrElse(-1) }
            .filter(_ != -1)

          if (ifPrint) println(s"  PE($from) -> { ${destinations.mkString(", ")} }")

          (from, destinations)
        }.toMap
      }


    
    println("\nOutput Data 1 Routes (outSetD1), mapped from PE ID:")
    val peIdToDestinationsD1: Map[Int, Seq[Int]] =
      if (peIdToOutSetD1.isEmpty) {
        if (ifPrint) println("  (No data found)")
        Map.empty[Int, Seq[Int]]
      } else {
        peIdToOutSetD1.toSeq.sortBy(_._1).map { case (from, toArray) =>
          val destinationList = getOrderedDestinations(from, rows, cols)
          if (ifPrint) println(s"  PE($from) destination list: $destinationList")

          val destinations = toArray.toSeq.zipWithIndex  
            .filter(_._1 == 1)
            .map { case (_, index) => destinationList.lift(index).getOrElse(-1) }
            .filter(_ != -1)

          if (ifPrint) println(s"  PE($from) -> { ${destinations.mkString(", ")} }")

          (from, destinations)
        }.toMap
      }

    println("\nOutput Predicate 0 Routes (outSetP0), mapped from PE ID:")
    val peIdToDestinationsP0: Map[Int, Seq[Int]] =
      if (peIdToOutSetP0.isEmpty) {
        if (ifPrint) println("  (No data found)")
        Map.empty[Int, Seq[Int]]
      } else {
        peIdToOutSetP0.toSeq.sortBy(_._1).map { case (from, toArray) =>
          val destinationList = getOrderedDestinations(from, rows, cols)
          if (ifPrint) println(s"  PE($from) destination list: $destinationList")

          val destinations = toArray.toSeq.zipWithIndex   
            .filter(_._1 == 1)
            .map { case (_, index) => destinationList.lift(index).getOrElse(-1) }
            .filter(_ != -1)

          if (ifPrint) println(s"  PE($from) -> { ${destinations.mkString(", ")} }")

          (from, destinations)
        }.toMap
      }


    println("\nOutput Predicate 1 Routes (outSetP1), mapped from PE ID:")
    val peIdToDestinationsP1: Map[Int, Seq[Int]] =
      if (peIdToOutSetP1.isEmpty) {
        if (ifPrint) println("  (No data found)")
        Map.empty[Int, Seq[Int]]
      } else {
        peIdToOutSetP1.toSeq.sortBy(_._1).map { case (from, toArray) =>
          val destinationList = getOrderedDestinations(from, rows, cols)
          if (ifPrint) println(s"  PE($from) destination list: $destinationList")

          val destinations = toArray.toSeq.zipWithIndex  
            .filter(_._1 == 1)
            .map { case (_, index) => destinationList.lift(index).getOrElse(-1) }
            .filter(_ != -1)

          if (ifPrint) println(s"  PE($from) -> { ${destinations.mkString(", ")} }")

          (from, destinations)
        }.toMap
      }    

    if (if_print) {
      println("\nFinal Placements (finalplacements), mapped from Instruction Index:")
      if (tempFinalPlacements.isEmpty) {
        println("  (No data found)")
      } else {
        tempFinalPlacements.toSeq.sortBy(_._1).foreach { case (instIndex, peId) =>
          println(s"Instruction($instIndex): PE($peId)")
        }
      }
    }


    CgraConfiguration(peIdToInstArray.toMap, peIdToDestinationsD0.toMap, peIdToDestinationsD1.toMap, peIdToDestinationsP0.toMap, peIdToDestinationsP1.toMap, tempFinalPlacements.toMap)
  }


  // ---------- FSM framework ----------
  case class FSMResult(dataOut: Option[Int] = None, predOut: Option[Int] = None)

  /** Minimal stateful interface for instructions that need cycle-to-cycle state. */
  trait StatefulInstruction {
    /** Step one cycle of the FSM.
      * @param input0 first data input (Int, may be int-bits-for-float)
      * @param input1 second data input
      * @param predIn predicate input
      * @return FSMResult with optional data/predicate outputs for this cycle
      */
    var state: PhiLoopState.Value
    def step(input0: Int, input1: Int, predIn: Int, input0_valid: Boolean, input1_valid: Boolean, predIn_valid: Boolean): FSMResult
  }

  // ---------- FSM implementations ----------

  // PHI_LOOP_DATA (two-state)
  object PhiLoopState extends Enumeration { val Initial, Loop = Value }
  import PhiLoopState._
  class PhiLoopFSM extends StatefulInstruction {
    var state: PhiLoopState.Value = Initial
    var lastOutput: Int = 0

    def step(input0: Int, input1: Int, predIn: Int, input0_valid: Boolean, input1_valid: Boolean, predIn_valid: Boolean): FSMResult = {
      state match {
        case Initial =>   
          if (input0_valid)
          {
            lastOutput = input0
            state = Loop
            FSMResult(dataOut = Some(input0), predOut = Some(1))
          }  
          else {
            FSMResult()
          }         
          

        case Loop =>
          if (predIn==1 && predIn_valid) {
            if (input1_valid)
            {
              lastOutput = input1
              FSMResult(dataOut = Some(lastOutput), predOut = Some(1))
            }
            else {
              FSMResult()
            }
          } else if (predIn==0 && predIn_valid && input1_valid) {
            // break loop, discard input1, return to initial
            state = Initial
            FSMResult()
          }
          else {
            FSMResult()
          }
      }
    }
  }



  // LOOPOUT (forward data when predicate true)
  class LoopoutFSM extends StatefulInstruction {
    override var state: PhiLoopState.Value = PhiLoopState.Initial
    def step(input0: Int, input1: Int, predIn: Int, input0_valid: Boolean, input1_valid: Boolean, predIn_valid: Boolean): FSMResult = {
      if (predIn==1 && predIn_valid && input0_valid) FSMResult(dataOut = Some(input0), predOut = Some(input0)) else FSMResult()
    }
  }



  // ---------- FSM factory ----------
  def createFSMs(): Map[InstructionType.Value, StatefulInstruction] = {
    Map(
      InstructionType.PHI_LOOP_DATA -> new PhiLoopFSM(),
      InstructionType.PHI_LOOP_PRED -> new PhiLoopFSM(),
      InstructionType.LOOP_OUT_DATA -> new LoopoutFSM(),
      InstructionType.LOOP_OUT_PRED -> new LoopoutFSM() // or specialized version if needed
    )
  }


  /**
   * The main CGRA simulator class.
   *
   * @param rows The number of rows in the PE array.
   * @param cols The number of columns in the PE array.
   * @param config The `CgraConfiguration` containing instructions and routing.
   */
  class CGRA(val rows: Int, val cols: Int, config: CgraConfiguration) {
    private val peArray: Array[Array[PE]] = Array.ofDim[PE](rows, cols)
    private var globalCycle: Long = 0
    private val nocAdjacencyList: MutableMap[Int, MutableSet[Int]] = MutableMap.empty

    // --- Initialize Memory ---
    val memory: Memory = new Memory(size = 65536*16, readPorts = 4, writePorts = 4, accessLatency = 2)

    // Initialize PEs and build NoC
    for {
      r <- 0 until rows
      c <- 0 until cols
    } {
      val id = r * cols + c
      val rawInstBits = config.peIdToInstArray.getOrElse(id, Array.fill(64)(0))
      val rawInst = parseBitArrayToLong(rawInstBits)
      
      if (if_print) print("PE",id,": ")
      if (if_print) println(rawInst.toBinaryString)
      val instruction = parseInstructionFromLong(rawInst)
      // println(instruction.opType)
      val fsmMap = createFSMs()
      peArray(r)(c) = PE(id, r, c, instruction, fsms = fsmMap)
      nocAdjacencyList(id) = MutableSet(getOrderedDestinations(id, rows, cols): _*)
    }

    /**
     * Finds a PE by its ID.
     * @param id The ID of the PE.
     * @return The `PE` object or `None` if not found.
     */
    def getPE(id: Int): Option[PE] = {
      val r = id / cols
      val c = id % cols
      if (r < rows && c < cols) Some(peArray(r)(c)) else None
    }

    def boolToInt(b: Boolean): Int = if (b) 1 else 0


    /**
     * Executes one cycle of the simulation.
     */
    def step(): Unit = {
      println(s"--- Global Cycle: $globalCycle ---")
      // var datain0_valid = false
      // var datain1_valid = false
      // var predin0_valid = false
      // var predin1_valid = false

      // var loop_out_valid = false
      // var phi_loop_valid = false 
      // var null_valid = false

      // var execute_once = false

      memory.step()
      // ROUND 1: Read inputs and execute instructions
      println("Round 1: Execution Phase")
      val peExecutionQueue = new ArrayBuffer[PE]()
      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        val pe = peArray(r)(c)
        // Check if the PE is ready to run
        if (pe.state == CanRun) {
          val instruction = pe.instruction
          var canExecute = true

          if (globalCycle == 0) {
            println(s"  PE(${pe.id}) ${pe.instruction}")
          }
          //the last two bits (1-0) show whether has predicate input, 00 for no, 01 for predicate in0, 10 for both, 11 for reverse predate in0.
          //the (3-2) bit indicate whether has predicate out or data out, 00 for no out, 01 for only data, 10 for only predicate, 11 for both
          //(6-4) indicate data input, 000 for no, 001 for only data in0, 010 for both data, 011 for data in0 and imm, 100 for imm and data in0, 101 for imm and data in1, 110 for Imm only
          
          //(15-13) represents which pred comes from Imm,01 for pred0, 10 for pred1, 11 for special case, pred0 is a Imm and only produce once, 100 for special case, data0 is a Imm and only produce once
          // pe.datain0_valid = pe.inDataFifo0.nonEmpty
          // pe.datain1_valid = pe.inDataFifo1.nonEmpty
          // pe.predin0_valid = pe.inPredFifo0.nonEmpty
          // pe.predin1_valid = pe.inPredFifo1.nonEmpty

          // data valid
          //(6-4) indicate data input, 000 for no, 001 for only data in0, 010 for both data, 011 for data in0 and imm, 100 for imm and data in0, 101 for imm and data in1, 110 for Imm only
          instruction.dataInConfig match {
            case 1 => pe.datain0_valid = if (pe.inDataFifo0.nonEmpty) true else false
            case 2 => pe.datain0_valid = if (pe.inDataFifo0.nonEmpty) true else false
                      pe.datain1_valid = if (pe.inDataFifo1.nonEmpty) true else false
            case 3 => pe.datain0_valid = if (pe.inDataFifo0.nonEmpty) true else false
                      pe.datain1_valid = true
            case 4 => pe.datain0_valid = true
                      pe.datain1_valid = if (pe.inDataFifo0.nonEmpty) true else false
            case 5 => pe.datain0_valid = true
                      pe.datain1_valid = if (pe.inDataFifo1.nonEmpty) true else false
            case 6 => pe.datain0_valid = true
                      pe.datain1_valid = true
            case _ => // No data input, no check needed
          }

          //pred valid
          //the last two bits (1-0) show whether has predicate input, 00 for no, 01 for predicate in0, 10 for both, 11 for reverse predate in0.
          instruction.predInConfig match {
            case 1 => pe.predin0_valid = if (pe.inPredFifo0.nonEmpty) true else false
            case 2 => pe.predin0_valid = if (pe.inPredFifo0.nonEmpty) true else false
                      pe.predin1_valid = if (pe.inPredFifo1.nonEmpty) true else false
            case 3 => pe.predin0_valid = if (pe.inPredFifo0.nonEmpty) true else false
            case _ => // No predicate input, no check needed
          }

          if (instruction.immPredConfig == 3) {
            assert(instruction.opType == InstructionType.PHI_LOOP_PRED || instruction.opType == InstructionType.NULL)
            pe.predin0_valid = true
          }

          // Check for data inputs
          instruction.dataInConfig match {
            case 1 => if (pe.inDataFifo0.isEmpty) canExecute = false
            case 2 => if (pe.inDataFifo0.isEmpty || pe.inDataFifo1.isEmpty) canExecute = false
            case 3 => if (pe.inDataFifo0.isEmpty) canExecute = false
            case 4 => if (pe.inDataFifo0.isEmpty) canExecute = false
            case 5 => if (pe.inDataFifo1.isEmpty) canExecute = false
            case 6 => // Immediate only, no fifo check needed
            case _ => // No data input, no check needed
          }
          // Check for predicate inputs
          instruction.predInConfig match {
            case 1 => if (pe.inPredFifo0.isEmpty) canExecute = false
            case 2 => if (pe.inPredFifo0.isEmpty || pe.inPredFifo1.isEmpty) canExecute = false
            case 3 => if (pe.inPredFifo0.isEmpty) canExecute = false
            case _ => // No predicate input, no check needed
          }

          

          // only imm
          if ((instruction.dataInConfig == 6 || instruction.dataInConfig == 0) &&
              (instruction.predInConfig == 0)) {
            pe.execute_once = true
          }

          // var route_data_valid = false
          // var route_pred_valid = false
          if (instruction.opType == InstructionType.ROUTE) {
            if (instruction.dataInConfig == 1 && instruction.predInConfig == 0) {
              canExecute = pe.datain0_valid
              pe.route_data_valid = true
            }
            else if (instruction.dataInConfig == 1 && instruction.predInConfig == 1) {
              canExecute = pe.datain0_valid && pe.predin0_valid
              pe.route_data_valid = true && (instruction.outConfig == 1 || instruction.outConfig == 3)
              pe.route_pred_valid = true && (instruction.outConfig == 2 || instruction.outConfig == 3)
            }
            else if (instruction.predInConfig == 1 && (instruction.dataInConfig == 0 || instruction.dataInConfig == 6)) {
              canExecute = pe.predin0_valid
              pe.route_pred_valid = true
            }
          }

          if (instruction.opType == InstructionType.MERGE_PRED) {
            canExecute = pe.predin0_valid && pe.predin1_valid
          }

          if (instruction.opType == InstructionType.PHI_LOOP_DATA) {
            val fsm = pe.fsms(InstructionType.PHI_LOOP_DATA).asInstanceOf[PhiLoopFSM]
            canExecute = fsm.state match {
              case Initial =>
                pe.datain0_valid

              case Loop =>
                pe.predin0_valid && pe.datain1_valid
            }
          }

          if (instruction.opType == InstructionType.PHI_LOOP_PRED) {
            val fsm = pe.fsms(InstructionType.PHI_LOOP_PRED).asInstanceOf[PhiLoopFSM]
            if (instruction.immPredConfig == 3) { //pred0 is a Imm and only produce once
              canExecute = fsm.state match {
                case Initial =>
                  true

                case Loop =>
                  pe.datain0_valid && pe.predin1_valid
              }
            } else {
              canExecute = fsm.state match {
                case Initial =>
                  pe.predin0_valid

                case Loop =>
                  pe.datain0_valid && pe.predin1_valid
              }
            }
          }


          if (instruction.opType == InstructionType.LOOP_OUT_DATA) {
            canExecute = pe.predin0_valid && pe.datain0_valid
          }

          if (instruction.opType == InstructionType.LOOP_OUT_PRED) {
            canExecute = pe.predin0_valid && pe.datain0_valid
          }

          if (pe.currentLoadReq.isDefined || pe.currentStoreReq.isDefined) {
            canExecute = true //excute to query the memory but dont dequeue
          }

          //(15-13) represents which pred comes from Imm,01 for pred0, 10 for pred1, 11 for special case, pred0 is a Imm and only produce once, 100 for special case, data0 is a Imm and only produce once
          if ((instruction.opType == InstructionType.PHI_BRANCH_PRED && instruction.immPredConfig == 1 && pe.inDataFifo0.nonEmpty && pe.inDataFifo0.front == 1)
            ||(instruction.opType == InstructionType.PHI_BRANCH_PRED && instruction.immPredConfig == 2 && pe.inDataFifo0.nonEmpty && pe.inDataFifo0.front == 0)) {
            canExecute = true
          }

          if ((instruction.opType == InstructionType.PHI_BRANCH_PRED && pe.inDataFifo0.nonEmpty && pe.inDataFifo0.front == 1 && pe.predin0_valid)
            ||(instruction.opType == InstructionType.PHI_BRANCH_PRED && pe.inDataFifo0.nonEmpty && pe.inDataFifo0.front == 0 && pe.predin1_valid)) {
              canExecute = true
            }

          //(6-4) indicate data input, 000 for no, 001 for only data in0, 010 for both data, 011 for data in0 and imm, 100 for imm and data in0, 101 for imm and data in1, 110 for Imm only
          if ((instruction.opType == InstructionType.PHI_BRANCH_DATA && pe.inPredFifo0.nonEmpty && pe.inPredFifo0.front == 1 && pe.datain0_valid)
            ||(instruction.opType == InstructionType.PHI_BRANCH_DATA && pe.inPredFifo0.nonEmpty && pe.inPredFifo0.front == 0 && pe.datain1_valid)) {
            canExecute = true
          }

          if (canExecute && pe.instruction.opType != InstructionType.NULL) {
            if (if_print) println(s"  PE(${pe.id}) ${pe.instruction} is ready to execute.")
            peExecutionQueue += pe
          } else if (pe.instruction.opType != InstructionType.NULL){
            if (if_print) println(s"  PE(${pe.id}) ${pe.instruction} is stalled due to insufficient inputs.")
          } else {
            if (if_print) println(s"  PE(${pe.id}) ${pe.instruction} is null.")
          }
        } else {
          if (if_print) println(s"  PE(${pe.id}) is stalled.")
        }
      }

      // Perform execution for all PEs that can run
      peExecutionQueue.foreach { pe =>
        val instruction = pe.instruction
        
        // Consume from FIFOs based on instruction
        // print("623")
        // print(instruction) 
        // println("currentLoadReq:",pe.currentLoadReq.isEmpty)
        // println("currentStoreReq",pe.currentStoreReq.isEmpty)
        var data0: Int = 0
        var data1: Int = 0
        var pred0: Int = 0
        var pred1: Int = 0

        //(6-4) indicate data input, 000 for no, 001 for only data in0, 010 for both data, 011 for data in0 and imm, 100 for imm and data in0, 101 for imm and data in1, 110 for Imm only
        //the last two bits (1-0) show whether has predicate input, 00 for no, 01 for predicate in0, 10 for both, 11 for reverse predate in0.
        //(15-13) represents which pred comes from Imm,01 for pred0, 10 for pred1, 11 for special case, pred0 is a Imm and only produce once, 100 for special case, data0 is a Imm and only produce once
        if (instruction.opType == InstructionType.PHI_LOOP_DATA) {
          if (pe.current_state == PhiLoopState.Initial) {
            if (instruction.dataInConfig == 1 || instruction.dataInConfig == 2 || instruction.dataInConfig == 3) {
              if (pe.inDataFifo0.isEmpty) println(s"pe(${pe.id}).inDataFifo0.isEmpty")
              data0 = pe.inDataFifo0.dequeue() 
            }
            else if (instruction.dataInConfig == 1 || instruction.dataInConfig == 2 || instruction.dataInConfig == 3) {
              data0 = instruction.immediateValue
            }
          } else {
            if (instruction.dataInConfig == 2 || instruction.dataInConfig == 5) {
              if (pe.inDataFifo1.isEmpty) println(s"pe(${pe.id}).inDataFifo1.isEmpty")
              data1 = pe.inDataFifo1.dequeue() 
            } else {
              data1 = instruction.immediateValue
            }
            if (instruction.immPredConfig == 1 || instruction.immPredConfig == 3) {
              pred0 = instruction.immediateValue
            } else {
              if (pe.inPredFifo0.isEmpty) println(s"pe(${pe.id}).inPredFifo0.isEmpty")
              pred0 = pe.inPredFifo0.dequeue()
            }
          }
        } else if (instruction.opType == InstructionType.PHI_LOOP_PRED) {
          //(15-13) represents which pred comes from Imm,01 for pred0, 10 for pred1, 11 for special case, pred0 is a Imm and only produce once, 100 for special case, data0 is a Imm and only produce once
          if (pe.current_state == PhiLoopState.Initial) {
            if (instruction.immPredConfig == 1 || instruction.immPredConfig == 3) {
              pred0 = instruction.immediateValue
            } else {
              if (pe.inPredFifo0.isEmpty) println(s"pe(${pe.id}).inPredFifo0.isEmpty")
              pred0 = pe.inPredFifo0.dequeue()
            }
          } else { //Loop
            if (instruction.immPredConfig == 2) {
              pred1 = instruction.immediateValue
            } else {
              if (pe.inPredFifo1.isEmpty) println(s"pe(${pe.id}).inPredFifo1.isEmpty")
              pred1 = pe.inPredFifo1.dequeue()
              
              
              
              
            }
            if (pe.inDataFifo0.isEmpty) println(s"pe(${pe.id}).inDataFifo0.isEmpty")
                data0 = pe.inDataFifo0.dequeue() 
          }
        } else if (instruction.opType == InstructionType.PHI_BRANCH_DATA) {
          if (pe.inPredFifo0.front == 1) {
            if (instruction.dataInConfig == 1 || instruction.dataInConfig == 2 || instruction.dataInConfig == 3) {
              if (pe.inDataFifo0.isEmpty) println(s"pe(${pe.id}).inDataFifo0.isEmpty")
              data0 = pe.inDataFifo0.dequeue() 
            }
            else {
              data0 = instruction.immediateValue
            }
          } else if (pe.inPredFifo0.front == 0) {
            if (instruction.dataInConfig == 2 || instruction.dataInConfig == 5) {
              if (pe.inDataFifo1.isEmpty) println(s"pe(${pe.id}).inDataFifo1.isEmpty")
              data1 = pe.inDataFifo1.dequeue() 
            } else {
              data1 = instruction.immediateValue
            }
          }
          if (instruction.immPredConfig == 1 || instruction.immPredConfig == 3) {
              pred0 = instruction.immediateValue
            } else {
              if (pe.inPredFifo0.isEmpty) println(s"pe(${pe.id}).inPredFifo0.isEmpty")
              pred0 = pe.inPredFifo0.dequeue()
            }
        } else if (instruction.opType == InstructionType.PHI_BRANCH_PRED) {
          if (pe.inDataFifo0.front == 1) {
            if (instruction.immPredConfig == 1 || instruction.immPredConfig == 3) {
              pred0 = instruction.immediateValue
            } else {
              if (pe.inPredFifo0.isEmpty) println(s"pe(${pe.id}).inPredFifo0.isEmpty")
              pred0 = pe.inPredFifo0.dequeue()
            }
          } else if (pe.inDataFifo0.front == 0) {
            if (instruction.immPredConfig == 2) {
              pred1 = instruction.immediateValue
            } else {
            if (pe.inPredFifo1.isEmpty) println(s"pe(${pe.id}).inPredFifo1.isEmpty")
              pred1 = pe.inPredFifo1.dequeue()
            }
          }
          if (pe.inDataFifo0.isEmpty) println(s"pe(${pe.id}).inDataFifo0.isEmpty")
          data0 = pe.inDataFifo0.dequeue()
        } else {
          //(6-4) indicate data input, 000 for no, 001 for only data in0, 010 for both data, 011 for data in0 and imm, 100 for imm and data in0, 101 for imm and data in1, 110 for Imm only
          data0 = if (pe.currentLoadReq.isEmpty && pe.currentStoreReq.isEmpty) {
            instruction.dataInConfig match {
            case 1 | 2 | 3 | 4 => 
              if (pe.inDataFifo0.isEmpty) println(s"pe(${pe.id}).inDataFifo0.isEmpty")
              pe.inDataFifo0.dequeue() 
            case _ => 0
            }
          } else 0
          data1 = if (pe.currentLoadReq.isEmpty && pe.currentStoreReq.isEmpty) {
            instruction.dataInConfig match {
            case 2 | 5 =>
              if (pe.inDataFifo1.isEmpty) println(s"pe(${pe.id}).inDataFifo1.isEmpty")
              pe.inDataFifo1.dequeue() 
            case _ => 0
            }
          } else 0
          pred0 = if (pe.currentLoadReq.isEmpty && pe.currentStoreReq.isEmpty) {
            instruction.predInConfig match {
              case 1 | 2 | 3 =>
                if (pe.inPredFifo0.isEmpty) println(s"pe(${pe.id}).inPredFifo0.isEmpty")
                pe.inPredFifo0.dequeue()
              case _ => 0
            }
          } else 0
          pred1 = if (pe.currentLoadReq.isEmpty && pe.currentStoreReq.isEmpty) {
            instruction.predInConfig match {
              case 2 =>
                if (pe.inPredFifo1.isEmpty) println(s"pe(${pe.id}).inPredFifo1.isEmpty")
                pe.inPredFifo1.dequeue()
              case _ => 0
            }
          } else 0
        }


        var result: Int = 0
        var predResult: Int = 0

        var load_valid: Boolean = false
        var store_valid: Boolean = false

        //the last two bits (1-0) show whether has predicate input, 00 for no, 01 for predicate in0, 10 for both, 11 for reverse predate in0.
        //the (3-2) bit indicate whether has predicate out or data out, 00 for no out, 01 for only data, 10 for only predicate, 11 for both
        //(6-4) indicate data input, 000 for no, 001 for only data in0, 010 for both data, 011 for data in0 and imm, 100 for imm and data in0, 101 for imm and data in1, 110 for Imm only

        // Determine final inputs, considering immediates and predicate reversal
        var finalData0 = instruction.dataInConfig match {
          case 4 | 5 | 6 => instruction.immediateValue
          case 1 | 2 | 3 => data0
          case _ => 0
        }
        var finalData1 = instruction.dataInConfig match {
          case 3 => instruction.immediateValue
          case 4 => data0
          case 2 | 5 => data1
          case _ => 0
        }
        var finalPred0 = instruction.predInConfig match {
          case 1 | 2 => pred0
          case 3 => 1-pred0
          case _ => 0
        }
        var finalPred1 = instruction.predInConfig match {
          case 2 => pred1
          case _ => 0
        }

        if (instruction.opType == InstructionType.LOOP_OUT_PRED) {
          finalData0 = 1-finalData0
        }
        if (instruction.opType == InstructionType.LOOP_OUT_DATA) {
          finalPred0 = 1-finalPred0
        }

        //instruction type, 0 for add, 1 for sub, 2 for eq, 3 for lt, 4 for gt, 5 for and, 6 for or, 7 for not, 8 for left shift
        //9 for unsigend right shift, 10 for signed right shift, 11 for xor, 12 for mul, 13 for udiv, 14 for sdiv, 15 for urem, 16 for srem,
        //17 for gen, 18 for loopoutData, 19 for slt, 20 for sgt, 21 for fmul, 22 for fadd, 23 for fdiv, 24 for fsub, 25 for fp2int, 26 for int2fp
        //27 for load, 28 for store, 29 for phibranchData, 30 for philoopData, 31 for fornormal, 32 for forstep, 33 for forupper, 34 for forall
        //35 for phibranchPred, 36 for philoopPred, 37 for loopoutPred, 38 for uneq, 39 for leq, 40 for geq, 41 for sleq, 42 for sgeq, 43 for route, 44 for mergepred


        // Execute the instruction based on the opType
        instruction.opType match {
          case InstructionType.ADD => result = finalData0 + finalData1
          case InstructionType.SUB => result = finalData0 - finalData1
          case InstructionType.EQ => 
            predResult = boolToInt(finalData0 == finalData1)
            result = predResult
          case InstructionType.LT => 
            predResult = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) < 0)
            result = predResult
          case InstructionType.GT => 
            predResult = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) > 0)
            result = predResult
          case InstructionType.AND => 
            result = finalData0 & finalData1
            predResult = result
          case InstructionType.OR => 
            // print("or",(finalData0,finalData1))
            result = finalData0 | finalData1
            predResult = result
          case InstructionType.NOT => 
            result = ~finalData0
            predResult = result
          case InstructionType.LSHIFT => result = finalData0 << finalData1.toInt
          case InstructionType.URSHIFT => result = finalData0 >>> finalData1.toInt
          case InstructionType.SRSHIFT => result = finalData0 >> finalData1.toInt
          case InstructionType.XOR => 
            result = finalData0 ^ finalData1
            predResult = result
          case InstructionType.MUL => result = finalData0 * finalData1
          case InstructionType.UDIV => result = if (finalData1 == 0) 0 else java.lang.Integer.divideUnsigned(finalData0, finalData1)
          case InstructionType.SDIV => result = if (finalData1 == 0) 0 else finalData0 / finalData1
          case InstructionType.UREM => result = if (finalData1 == 0) 0 else java.lang.Integer.remainderUnsigned(finalData0, finalData1)
          case InstructionType.SREM => result = if (finalData1 == 0) 0 else finalData0 % finalData1
          case InstructionType.GEN => result = finalData0
          case InstructionType.LOOP_OUT_DATA =>
            val (resulttmp, predResulttmp, loopValid) = pe.fsms.get(InstructionType.LOOP_OUT_DATA) match {
              case Some(fsm) =>
                val fsmResult = fsm.step(finalData0, finalData1, finalPred0,
                                        pe.datain0_valid, pe.datain1_valid, pe.predin0_valid)
                (
                  fsmResult.dataOut.getOrElse(0),
                  fsmResult.predOut.getOrElse(0),
                  fsmResult.dataOut.isDefined
                )
              case None =>
                (0, 0, false)
            }
            result = resulttmp
            predResult = predResulttmp
            pe.loop_out_valid = loopValid
          case InstructionType.SLT => 
            predResult = boolToInt(finalData0 < finalData1)
            result = predResult
          case InstructionType.SGT => 
            predResult = boolToInt(finalData0 > finalData1)
            result = predResult
          case InstructionType.FMUL => result = java.lang.Float.floatToIntBits(java.lang.Float.intBitsToFloat(finalData0)*java.lang.Float.intBitsToFloat(finalData1))
          case InstructionType.FADD => result = java.lang.Float.floatToIntBits(
              java.lang.Float.intBitsToFloat(finalData0) +
              java.lang.Float.intBitsToFloat(finalData1)
            )
          case InstructionType.FSUB =>
            result = java.lang.Float.floatToIntBits(
              java.lang.Float.intBitsToFloat(finalData0) -
              java.lang.Float.intBitsToFloat(finalData1)
            )
          case InstructionType.FDIV =>
            result = java.lang.Float.floatToIntBits(
              java.lang.Float.intBitsToFloat(finalData0) /
              java.lang.Float.intBitsToFloat(finalData1)
            )
          case InstructionType.FP2INT => result = Math.round(java.lang.Float.intBitsToFloat(finalData0))
          case InstructionType.INT2FP => result = java.lang.Float.floatToIntBits(finalData0.toFloat)
          case InstructionType.LOAD => 
            if (pe.instruction.predInConfig == 1 && finalPred0 == 1 || pe.instruction.predInConfig == 0) { 
              if (pe.currentLoadReq.isEmpty) {
                pe.currentLoadReq = Some(memory.load(finalData0))
              }
            }
            if (pe.currentLoadReq.nonEmpty) {
              val req = pe.currentLoadReq.get
              if (req.valid) {
                result = req.data
                predResult = 1
                load_valid = true
                pe.currentLoadReq = None
                if (if_print) println(s"  PE(${pe.id}) load from addr ${req.addr} completed, value=${req.data}")
              } else {
                if (if_print) println(s"  PE(${pe.id}) load waiting...")
              }
              // print(pe.execute_once)
            }
          case InstructionType.STORE => 
            if (pe.instruction.predInConfig == 1 && finalPred0 == 1 || pe.instruction.predInConfig == 0) { 
              if (pe.currentStoreReq.isEmpty) {
                pe.currentStoreReq = Some(memory.store(finalData1, finalData0))
              }
            }
            if (pe.currentStoreReq.nonEmpty) {
              val req = pe.currentStoreReq.get
              if (req.valid) {
                if (if_print) println(s"  PE(${pe.id}) store completed to addr ${req.addr}")
                pe.currentStoreReq = None
                predResult = 1
                store_valid = true
              } else {
                if (if_print) println(s"  PE(${pe.id}) store waiting...")
              }
            }
          case InstructionType.PHI_BRANCH_DATA => result = if (finalPred0 == 1) finalData0 else finalData1
          case InstructionType.PHI_LOOP_DATA => 
            val (resulttmp, predResulttmp, loopValid) = pe.fsms.get(InstructionType.PHI_LOOP_DATA) match {
              case Some(fsm) =>
                if (if_print) print(finalData0, finalData1, finalPred0, pe.datain0_valid, pe.datain1_valid, pe.predin0_valid)
                if (if_print) print(s"current state: ${fsm.state}")
                val fsmResult = fsm.step(finalData0, finalData1, finalPred0, pe.datain0_valid, pe.datain1_valid, pe.predin0_valid)
                if (if_print) println(s"FSM result: dataOut=${fsmResult.dataOut}, predOut=${fsmResult.predOut} ")
                (
                  fsmResult.dataOut.getOrElse(0),
                  fsmResult.predOut.getOrElse(0),
                  fsmResult.dataOut.isDefined,
                )
              case None =>
                (0, 0, false)
              }
            result = resulttmp
            predResult = predResulttmp
            pe.phi_loop_valid = loopValid
            val fsm = pe.fsms(InstructionType.PHI_LOOP_DATA).asInstanceOf[PhiLoopFSM]
            pe.current_state = fsm.state
          // case InstructionType.FOR_NORMAL
          // case InstructionType.FOR_STEP
          // case InstructionType.FOR_UPPER
          // case InstructionType.FOR_ALL
          case InstructionType.PHI_BRANCH_PRED => 
            predResult = if (finalData0 == 1) finalPred0 else finalPred1
            if (instruction.immPredConfig == 1 || instruction.immPredConfig == 2 || instruction.immPredConfig == 3) {
              predResult = 1
            }
          case InstructionType.PHI_LOOP_PRED =>
            if (if_print) println(finalPred0, finalPred1, finalData0, pe.predin0_valid, pe.predin1_valid, pe.datain0_valid)
            if (pe.datain0_valid && pe.predin1_valid && finalData0 == 0 && instruction.immPredConfig == 3) {
              pe.instruction.opType = InstructionType.NULL
              pe.phi_loop_valid = false
              pe.phi_loop_imm_end = true
              if (if_print) println(s"phi loop imm end")
            } else {
              val (resulttmp, predResulttmp, loopValid) = pe.fsms.get(InstructionType.PHI_LOOP_PRED) match {
              case Some(fsm) =>
                val fsmResult = fsm.step(finalPred0, finalPred1, finalData0, pe.predin0_valid, pe.predin1_valid, pe.datain0_valid)
                (
                  fsmResult.dataOut.getOrElse(0),
                  fsmResult.predOut.getOrElse(0),
                  fsmResult.dataOut.isDefined,
                )            
              case None =>
                (0, 0, false)
              }
              result = resulttmp
              predResult = predResulttmp
              pe.phi_loop_valid = loopValid
              val fsm = pe.fsms(InstructionType.PHI_LOOP_PRED).asInstanceOf[PhiLoopFSM]
              pe.current_state = fsm.state
            }            
            if (instruction.immPredConfig == 1 || instruction.immPredConfig == 2 || instruction.immPredConfig == 3) {
              predResult = 1
            }
          case InstructionType.LOOP_OUT_PRED =>
            val (resulttmp, predResulttmp, loopValid) = pe.fsms.get(InstructionType.LOOP_OUT_PRED) match {
              case Some(fsm) =>
                val fsmResult = fsm.step(finalPred0, finalPred1, finalData0, pe.predin0_valid, pe.predin1_valid, pe.datain0_valid)
                (
                  fsmResult.dataOut.getOrElse(0),
                  fsmResult.predOut.getOrElse(0),
                  fsmResult.dataOut.isDefined,
                )
              case None =>
                (0, 0, false)
            }
            result = resulttmp
            predResult = predResulttmp
            pe.loop_out_valid = loopValid
          case InstructionType.UNEQ =>  // Unsigned Not Equal
            predResult = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) != 0)
            result = predResult
          case InstructionType.LEQ =>   // Unsigned Less than or Equal
            predResult = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) <= 0)
            result = predResult
          case InstructionType.GEQ =>   // Unsigned Greater than or Equal
            predResult = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) >= 0)
            result = predResult
          case InstructionType.SLEQ =>  // Signed Less than or Equal
            predResult = boolToInt(finalData0 <= finalData1)
            result = predResult
          case InstructionType.SGEQ =>  // Signed Greater than or Equal
            predResult = boolToInt(finalData0 >= finalData1)
            result = predResult
          case InstructionType.ROUTE =>
            predResult = finalPred0
            result = finalData0
          case InstructionType.MERGE_PRED =>
            if (pe.predin0_valid && pe.predin1_valid) {predResult = 1}
          case InstructionType.NULL => ()


          // Add other instructions as needed.
          case _ => println(s"  Warning: Unimplemented instruction type: ${instruction.opType}")
        }
        
        //the (3-2) bit indicate whether has predicate out or data out, 00 for no out, 01 for only data, 10 for only predicate, 11 for both
        // Store result in the temp buffer if it's supposed to produce an output
      val fsmTypes = Set(
        InstructionType.PHI_LOOP_DATA,
        InstructionType.PHI_LOOP_PRED,
        InstructionType.LOOP_OUT_DATA,
        InstructionType.LOOP_OUT_PRED
      )

      val outvalidtemp = instruction.predInConfig match {
          case 0 => true 
          case 1 => pe.predin0_valid && finalPred0 == 1 || pe.predin0_valid && instruction.opType == InstructionType.PHI_BRANCH_DATA
          case 2 => pe.predin0_valid && pe.predin1_valid || pe.datain0_valid && instruction.opType == InstructionType.PHI_BRANCH_PRED
          case 3 => pe.predin0_valid && finalPred0 == 1
          case _ => true
        }
      // println(s"pe.phi_loop_valid ${pe.phi_loop_valid}")
      if (fsmTypes.contains(instruction.opType)) {
        val fsmValid = instruction.opType match {
          case InstructionType.PHI_LOOP_DATA => pe.phi_loop_valid
          case InstructionType.PHI_LOOP_PRED => pe.phi_loop_valid
          case InstructionType.LOOP_OUT_DATA => pe.loop_out_valid
          case InstructionType.LOOP_OUT_PRED => pe.loop_out_valid
        }

        instruction.outConfig match {
          case 1 => 
            pe.tempOutBuffer = result
            pe.outValid = fsmValid
          case 2 => 
            pe.tempOutPredBuffer = predResult
            pe.outValid = fsmValid
          case 3 => 
            pe.tempOutBuffer = result
            pe.tempOutPredBuffer = predResult
            pe.outValid = fsmValid
          case _ => 
            pe.outValid = false
        }
      } else if (instruction.opType == InstructionType.ROUTE) {
        instruction.outConfig match {
          case 1 => 
            pe.tempOutBuffer = result
            pe.outValid = pe.route_data_valid
          case 2 => 
            pe.tempOutPredBuffer = predResult
            pe.outValid = pe.route_pred_valid
          case 3 => 
            pe.tempOutBuffer = result
            pe.tempOutPredBuffer = predResult
            pe.outValid = pe.route_data_valid && pe.route_pred_valid
          case _ => 
            pe.outValid = false
        }
      } else if (instruction.opType == InstructionType.LOAD) {
        instruction.outConfig match {
          case 1 => 
            pe.tempOutBuffer = result
            pe.outValid = load_valid
          case 2 => 
            pe.tempOutPredBuffer = predResult
            pe.outValid = load_valid
          case 3 => 
            pe.tempOutBuffer = result
            pe.tempOutPredBuffer = predResult
            pe.outValid = load_valid
          case _ => 
            pe.outValid = false
        }
      } else if (instruction.opType == InstructionType.STORE) {
        instruction.outConfig match {
          case 1 => 
            pe.tempOutBuffer = result
            pe.outValid = store_valid
          case 2 => 
            pe.tempOutPredBuffer = predResult
            pe.outValid = store_valid
          case 3 => 
            pe.tempOutBuffer = result
            pe.tempOutPredBuffer = predResult
            pe.outValid = store_valid
          case _ => 
            pe.outValid = false
        }
      }
      else {
        instruction.outConfig match {
          case 1 => 
            pe.tempOutBuffer = result
            pe.outValid = outvalidtemp
          case 2 => 
            pe.tempOutPredBuffer = predResult
            pe.outValid = outvalidtemp
          case 3 => 
            pe.tempOutBuffer = result
            pe.tempOutPredBuffer = predResult
            pe.outValid = outvalidtemp
          case _ => 
            pe.outValid = false
        }
      }
      if (pe.phi_loop_imm_end) {
        pe.outValid = false
      }

        if (pe.currentLoadReq.nonEmpty || pe.currentStoreReq.nonEmpty) {
          println(s"  PE(${pe.id}) (${pe.instruction}) ($finalData0,$finalData1,$finalPred0,$finalPred1,${pe.datain0_valid},${pe.datain1_valid},${pe.predin0_valid},${pe.predin1_valid}) executed. Result: data=$result, pred=$predResult. Velid: ${pe.outValid}")
        } else {
          println(s"  PE(${pe.id}) (${pe.instruction}) ($finalData0,$finalData1,$finalPred0,$finalPred1,${pe.datain0_valid},${pe.datain1_valid},${pe.predin0_valid},${pe.predin1_valid}) executed. Result: data=$result, pred=$predResult. Valid: ${pe.outValid}")
        }
      }


      // ROUND 2: Commit results to destination PEs
      println("Round 2: Commit Phase")
      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        val pe = peArray(r)(c)
        if (pe.outValid) {
          val instruction = pe.instruction
          val peId = pe.id
          val destinationList = getOrderedDestinations(peId, rows, cols)

          // The destination routing is now determined by the outSet arrays
          val outSetD0 = config.peIdToOutSetD0.getOrElse(peId, Seq.empty[Int])
          val outSetD1 = config.peIdToOutSetD1.getOrElse(peId, Seq.empty[Int])
          val outSetP0 = config.peIdToOutSetP0.getOrElse(peId, Seq.empty[Int])
          val outSetP1 = config.peIdToOutSetP1.getOrElse(peId, Seq.empty[Int])

          // println(s"Type of outSetD0: ${outSetD0.getClass}")
          // println(s"Contents of outSetD0: " + outSetD0.mkString(", "))  
          // println(s"Contents of outSetD1: " + outSetD1.mkString(", "))  
          // println(s"Contents of outSetP0: " + outSetP0.mkString(", "))  
          // println(s"Contents of outSetP1: " + outSetP1.mkString(", "))  

          val dataToRoute = instruction.outConfig match {
            case 1 => Some(pe.tempOutBuffer)
            case 3 => Some(pe.tempOutBuffer)
            case _ => None
          }
          val predToRoute = instruction.outConfig match {
            case 2 => Some(pe.tempOutPredBuffer)
            case 3 => Some(pe.tempOutPredBuffer)
            case _ => None
          }

          var successfullySent = false
          var anyDestinationFound =
            outSetD0.nonEmpty || outSetD1.nonEmpty || outSetP0.nonEmpty || outSetP1.nonEmpty

          // val canSendD0 = dataToRoute.isDefined && outSetD0.forall { destId =>
          //   print("destId:",destId)
          //   val peOpt = getPE(destId)
          //   peOpt match {
          //     case Some(pe) =>
          //       println(pe.inDataFifo0.size)
          //       val canAccept = pe.inDataFifo0.size < IN_FIFO_DEPTH
          //       if (!canAccept) {
          //         println(s"PE($destId) inDataFifo0 is full: size=${pe.inDataFifo0.size}")
          //       }
          //       canAccept
          //     case None =>
          //       println(s"PE($destId) not found")
          //       false
          //   }
          // }
          // println(canSendD0)

          val canSendD0 = if (dataToRoute.isDefined && outSetD0.nonEmpty) {
            val blocked = outSetD0.filter { destId =>
              getPE(destId).exists(_.inDataFifo0.size >= IN_FIFO_DEPTH)
            }
            if (blocked.nonEmpty) {
              if (if_print) println(s"[DEBUG] PE blocked at D0 routing: ${blocked.mkString(", ")}")
            }
            blocked.isEmpty
          } else true

          val canSendD1 = if (dataToRoute.isDefined && outSetD1.nonEmpty) {
            val blocked = outSetD1.filter { destId =>
              getPE(destId).exists(_.inDataFifo1.size >= IN_FIFO_DEPTH)
            }
            if (blocked.nonEmpty) {
              if (if_print) println(s"[DEBUG] PE blocked at D1 routing: ${blocked.mkString(", ")}")
            }
            blocked.isEmpty
          } else true

          val canSendP0 = if (predToRoute.isDefined && outSetP0.nonEmpty) {
            val blocked = outSetP0.filter { destId =>
              getPE(destId).exists(_.inPredFifo0.size >= IN_FIFO_DEPTH)
            }
            if (blocked.nonEmpty) {
              if (if_print) println(s"[DEBUG] PE blocked at P0 routing: ${blocked.mkString(", ")}")
            }
            blocked.isEmpty
          } else true

          val canSendP1 = if (predToRoute.isDefined && outSetP1.nonEmpty) {
            val blocked = outSetP1.filter { destId =>
              getPE(destId).exists(_.inPredFifo1.size >= IN_FIFO_DEPTH)
            }
            if (blocked.nonEmpty) {
              if (if_print) println(s"[DEBUG] PE blocked at P1 routing: ${blocked.mkString(", ")}")
            }
            blocked.isEmpty
          } else true


          if (anyDestinationFound && canSendD0 && canSendD1 && canSendP0 && canSendP1) {
            dataToRoute match {
              case Some(data) =>
                outSetD0.foreach { destId =>
                  getPE(destId) match {
                    case Some(destPE) =>
                      destPE.inDataFifo0.enqueue(data)
                      if (if_print) println(s"PE($peId) sent data0($data) to PE($destId)")
                    case None =>
                      if (if_print) println(s"Warning: PE($destId) not found, cannot send data0")
                  }
                }
                outSetD1.foreach { destId =>
                  getPE(destId) match {
                    case Some(destPE) =>
                      destPE.inDataFifo1.enqueue(data)
                      if (if_print) println(s"PE($peId) sent data1($data) to PE($destId)")
                    case None =>
                      if (if_print) println(s"Warning: PE($destId) not found, cannot send data1")
                  }
                }
              case None => ()
            }

            predToRoute match {
              case Some(pred) =>
                outSetP0.foreach { destId =>
                  getPE(destId) match {
                    case Some(destPE) =>
                      destPE.inPredFifo0.enqueue(pred)
                      if (if_print) println(s"PE($peId) sent pred0($pred) to PE($destId)")
                    case None =>
                      if (if_print) println(s"Warning: PE($destId) not found, cannot send pred0")
                  }
                }
                outSetP1.foreach { destId =>
                  getPE(destId) match {
                    case Some(destPE) =>
                      destPE.inPredFifo1.enqueue(pred)
                      if (if_print) println(s"PE($peId) sent pred1($pred) to PE($destId)")
                    case None =>
                      if (if_print) println(s"Warning: PE($destId) not found, cannot send pred1")
                  }
                }
              case None => ()
            }
            successfullySent = true
          } else {
            successfullySent = false
            if (dataToRoute.isDefined) {
              if (!canSendD0) {if (if_print) println(s"PE($peId) stalled: data0 FIFO full")}
              if (!canSendD1) {if (if_print) println(s"PE($peId) stalled: data1 FIFO full")}
            }
            if (predToRoute.isDefined) {
              if (!canSendP0) {if (if_print) println(s"PE($peId) stalled: pred0 FIFO full")}
              if (!canSendP1) {if (if_print) println(s"PE($peId) stalled: pred1 FIFO full")}
            }
          }



          if (successfullySent || !anyDestinationFound) {
            pe.outValid = false
            pe.state = CanRun
            if (pe.execute_once) {
              pe.instruction.opType = InstructionType.NULL
            }
          } else {
            pe.state = Stall
          }
        } else {
          pe.state = CanRun
        }
      }

      globalCycle += 1
      println("--- End of Cycle ---")
    }

    def isProgramFinished(): Boolean = {
      var i=0
      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        val pe = peArray(r)(c)
        if (pe.inDataFifo0.nonEmpty ||
            pe.inDataFifo1.nonEmpty ||
            pe.inPredFifo0.nonEmpty ||
            pe.inPredFifo1.nonEmpty ||
            pe.currentStoreReq.nonEmpty ||
            pe.currentLoadReq.nonEmpty) {
              
          return false
        }
      }
      true
    }


    /**
     * Run the simulation for a specified number of cycles.
     */
    def run(cycles: Int): Unit = {
      for (i <- 0 until cycles) {
        step()
        if (isProgramFinished()) {
          println(s"Program finished at cycle $i")
          return
        }
      }
    }

    // def run(cycles: Int): Unit = {
    //   for (_ <- 0 until cycles) {
    //     step()
    //   }
    // }
  }

}

object Main {
  def main(args: Array[String]): Unit = {
    println("Starting CGRA Simulator...")

    // val fileName = if (args.nonEmpty) args(0) else "arrays_output.txt"
    val fileName = "arrays_output.txt"
    // GetTanhTest(fileName, cycles = 8000)
    // MERGESORTTest()
    // NWTest()
    KmpTest()
  }

  def NWTest(fileName: String = "nw.ll_arrays_output.txt", cycles: Int = 200000): Unit = {
    val cgraRows = 23
    val cgraCols = 23

    try {
      val simu = new CgraSimulator(cgraRows, cgraCols)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      var msize0:Int=64
      var msize1:Int=64

      var ALEN=msize0
      var BLEN=msize1
      var params=8
      var SEQA:Array[Int]=new Array[Int](msize0)
      var SEQB:Array[Int]=new Array[Int](msize1)
      var alignedA:Array[Int]=new Array[Int](msize0+msize1)
      var alignedB:Array[Int]=new Array[Int](msize0+msize1)
      var M:Array[Int]=new Array[Int]((msize0+1)*(msize1+1))
      var ptr:Array[Int]=new Array[Int]((msize0+1)*(msize1+1))

      var alignedAbk:Array[Int]=new Array[Int](msize0+msize1)
      var alignedBbk:Array[Int]=new Array[Int](msize0+msize1)
      var Mbk:Array[Int]=new Array[Int]((msize0+1)*(msize1+1))
      var ptrbk:Array[Int]=new Array[Int]((msize0+1)*(msize1+1))
      ///////////////////////Init input arrays
      for(i<-0 to msize0-1){
          SEQA(i)=Random.nextInt(256)
          // println("SEQA ",i,SEQA(i))
      }
      for(i<-0 to msize1-1){
          SEQB(i)=Random.nextInt(256)
          // println("SEQB ",i,SEQB(i))
      }
      for(i<-0 to msize0+msize1-1){
          alignedA(i)=Random.nextInt(256)
          alignedAbk(i)=alignedA(i)
          alignedB(i)=Random.nextInt(256)
          alignedBbk(i)=alignedB(i)
      }
      for(i<-0 to (msize0+1)*(msize1+1)-1){
          M(i)=Random.nextInt(256)
          Mbk(i)=M(i)
          ptr(i)=Random.nextInt(256)
          ptrbk(i)=ptr(i)
      }
      ///////////////// local params
      
      var score:Int=0
      var up_left:Int=0
      var up:Int=0
      var left:Int=0
      var max:Int=0
      var row:Int=0
      var row_up:Int=0
      var r:Int=0
      var a_idx:Int=0
      var b_idx:Int=0
      var a_str_idx:Int=0
      var b_str_idx:Int=0
      score=1
      var pandu:Int=1;
      //////////////////main function
      println(s"HLS c simulation start") 
      // m=1
      var iternum=0
      var maxiter=0
      for(a_idx<-0 to ALEN){
          M(a_idx) = a_idx * (-1);
      }
      for(b_idx<-0 to BLEN){
          M(b_idx*(ALEN+1)) = b_idx * (-1);
      }

      // Matrix filling loop
      for(b_idx<-1 to BLEN){
        for(a_idx<-1 to ALEN){
          if(SEQA(a_idx-1) == SEQB(b_idx-1)){
              score = 1;
          } else {
              score = -1;
          }

          row_up = (b_idx-1)*(ALEN+1);
          row = (b_idx)*(ALEN+1);

          up_left = M(row_up + (a_idx-1)) + score;
          up      = M(row_up + (a_idx  )) -1;
          left    = M(row    + (a_idx-1)) -1;
          if(up>left){
              max=up;
          }
          else{
              max=left;
          }

          if(up_left>max){
              max=up_left;
          }
          

          
          

          M(row + a_idx) = max;
          if(max == left){
              ptr(row + a_idx) = 60;
          } else if(max == up){
              ptr(row + a_idx) = 94;
          } else{
              ptr(row + a_idx) = 92;
          }
        }
      }

      // TraceBack (n.b. aligned sequences are backwards to avoid string appending)
      a_idx = ALEN;
      b_idx = BLEN;
      a_str_idx = 0;
      b_str_idx = 0;
      if(a_idx>0){
          pandu=1;
      }
      else if(b_idx>0){
          pandu=1;
      }
      else{
          pandu=0;
      }
      while(pandu>0) {
          r = b_idx*(ALEN+1);
          maxiter=maxiter+1
          if (ptr(r + a_idx) == 92){
              alignedA(a_str_idx) = SEQA(a_idx-1);
              a_str_idx=a_str_idx+1;
              alignedB(b_str_idx) = SEQB(b_idx-1);
              b_str_idx=b_str_idx+1;
              a_idx=a_idx-1;
              b_idx=b_idx-1;
          }
          else if (ptr(r + a_idx) == 60){
              alignedA(a_str_idx) = SEQA(a_idx-1)
              alignedB(b_str_idx) = 45;
              a_str_idx=a_str_idx+1;
              b_str_idx=b_str_idx+1;
              a_idx=a_idx-1;
          }
          else{ // SKIPA
              alignedA(a_str_idx) = 45;
              alignedB(b_str_idx) = SEQB(b_idx-1)
              a_str_idx=a_str_idx+1;
              b_str_idx=b_str_idx+1;
              b_idx=b_idx-1;
          }
          if(a_idx>0){
              pandu=1;
          }
          else if(b_idx>0){
              pandu=1;
          }
          else{
              pandu=0;
          }
      }

      // Pad the result
      var u=a_str_idx
      var v=b_str_idx
      while(a_str_idx<ALEN+BLEN){
          alignedA(a_str_idx) = 95;
          a_str_idx=a_str_idx+1
      }
      while(b_str_idx<ALEN+BLEN){
          alignedB(b_str_idx) = 95;
          b_str_idx=b_str_idx+1
      }
      
      for(i<- 0 to ALEN+BLEN-1){
          println("a results",alignedA(i))
      }
      for(i<- 0 to ALEN+BLEN-1){
          println("b results",alignedB(i))
      }

      simulator.memory.mem(1) = 1+params
      simulator.memory.mem(2) = 1+params+msize0
      simulator.memory.mem(3) = params+1+msize0+msize1
      simulator.memory.mem(4) = params+1+msize0+msize1+msize0+msize1
      simulator.memory.mem(5) = params+1+msize0+msize1+msize0+msize1+msize0+msize1
      simulator.memory.mem(6) = params+1+msize0+msize1+msize0+msize1+msize0+msize1+(msize0+1)*(msize1+1)
      simulator.memory.mem(7) = msize0
      simulator.memory.mem(8) = msize1

      for (i<-0 to msize0-1) {
        simulator.memory.mem(params+1+i) = SEQA(i)
      }
      for (i<-0 to msize1-1) {
        simulator.memory.mem(params+1+i+msize0) = SEQB(i)
      }
      for (i<-0 to msize0+msize1-1) {
        simulator.memory.mem(params+1+i+msize0+msize1) = alignedAbk(i)
      }
      for (i<-0 to msize0+msize1-1) {
        simulator.memory.mem(params+1+i+msize0+msize1+msize0+msize1) = alignedBbk(i)
      }
      for(i<- 0 to (msize0+1)*(msize1+1)-1){
        simulator.memory.mem((params+1+msize0+msize1+msize0+msize1+msize0+msize1+i))=Mbk(i)//write real's number
          

      }
      for(i<- 0 to (msize0+1)*(msize1+1)-1){
        simulator.memory.mem((params+1+msize0+msize1+msize0+msize1+msize0+msize1+i+(msize0+1)*(msize1+1)))=ptrbk(i)//write real's number
          

      }
      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int]((msize0+1)*(msize1+1))

      // for(i<- 0 to msize-1) {
      //   realResult(i) = simulator.memory.mem(1+params+i)
      // }
      for(i<- 0 to (msize0+1)*(msize1+1)-1){
        realResult(i)=simulator.memory.mem((params+1+msize0+msize1+msize0+msize1+msize0+msize1+i))//write real's number
      }

      var pass = true
      for (i <- 0 until (msize0+1)*(msize1+1)) {
        val diff = math.abs(M(i) - realResult(i))
        if (diff > 1e-5) {
          // println(f"Mismatch at index $i: expected=${M(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      if (pass) println("All M values match!")
      else println("Some M values differ at M.")

      for(i<- 0 to (msize0+1)*(msize1+1)-1){
        realResult(i)=simulator.memory.mem((params+1+msize0+msize1+msize0+msize1+msize0+msize1+i+(msize0+1)*(msize1+1)))//write real's number
      }

      pass = true
      for (i <- 0 until (msize0+1)*(msize1+1)) {
        val diff = math.abs( ptr(i) - realResult(i))
        if (diff > 1e-5) {
          // println(f"Mismatch at index $i: expected=${ptr(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      if (pass) println("All ptr values match!")
      else println("Some ptr values differ.")


      for(i<- 0 to msize0+msize1-1){
        realResult(i)=simulator.memory.mem(((params+1+msize0+msize1+i)))//write real's number
      }

      pass = true
      for (i <- 0 until msize0+msize1) {
        val diff = math.abs( alignedA(i) - realResult(i))
        if (diff > 1e-5) {
          // println(f"Mismatch at index $i: expected=${alignedA(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      if (pass) println("All alignedA values match!")
      else println("Some alignedA values differ.")

      for(i<- 0 to msize0+msize1-1){
        realResult(i)=simulator.memory.mem(((params+1+2*(msize0+msize1)+i)))//write real's number
      }

      pass = true
      for (i <- 0 until msize0+msize1) {
        val diff = math.abs( alignedB(i) - realResult(i))
        if (diff > 1e-5) {
          // println(f"Mismatch at index $i: expected=${alignedB(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      if (pass) println("All alignedB values match!")
      else println("Some alignedB values differ.")

      println("Simulation finished.")
    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The file '$fileName' was not found.")
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace()
    }
  }


  def MERGESORTTest(fileName: String = "mergesort_arrays_output.txt", cycles: Int = 80000): Unit = {
    val cgraRows = 16
    val cgraCols = 16

    try {
      val simu = new CgraSimulator(cgraRows, cgraCols)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var msize:Int=128
      var params=3
      var a:Array[Int]=new Array[Int](msize)
      var a_bk:Array[Int]=new Array[Int](msize)
      var temp:Array[Int]=new Array[Int](msize)
      ///////////////////////Init input arrays
      for(i<-0 to msize-1){
          a(i)=Random.nextInt(256)
          a_bk(i)=a(i)
          // println("to sort ",i," is ",a(i))
          
      }

      var start=0
      var stop=msize
      var i=0
      var m=0
      var from=0
      var mid=0
      var to=0
      //int temp[asize];
      var i0 = 0
      var j0 = 0
      var k0 = 0
      var tmp_j = 0
      var tmp_i = 0

      m=1
      var iternum=0
      var maxiter=0
      while(m<stop - start){
      // for (m = 1; m < stop - start; m = 2 * m) {
          i=start
          while(i<stop){
              iternum=iternum+1
          // for (i = start; i < stop; i = i + 2 * m) {
              from = i;
              mid = i + m - 1;
              to = i + 2 * m - 1;
              if (to < stop) {
                  for (i0 <- from until mid+1) {
                  // for (i0 = from; i0 <= mid; i0=i0+1) {
                      temp(i0) = a(i0) 
                  }
                  for (j0 <- mid + 1 until to+1) {
                  // for (j0 = mid + 1; j0 <= to; j0=j0+1) {
                      temp(mid + 1 + to - j0) = a(j0)
                  }

                  i0 = from
                  j0 = to

                  for (k0 <- from until to+1) {
                      maxiter=maxiter+1
                      tmp_j = temp(j0)
                      tmp_i = temp(i0)
                      if (tmp_j < tmp_i) {
                          a(k0) = tmp_j;
                          j0=j0-1;
                      }
                      else {
                          a(k0) = tmp_i;
                          i0=i0+1;
                      }
                  }
              }
              else {
                  for (i0 <- from until mid+1) {
                  // for (i0 = from; i0 <= mid; i0=i0+1) {
                      temp(i0) = a(i0) 
                  }
                  for (j0 <- mid + 1 until stop+1) {
                  // for (j0 = mid + 1; j0 <= to; j0=j0+1) {
                      temp(mid + 1 + stop - j0) = a(j0)
                  }

                  i0 = from
                  j0 = stop

                  for (k0 <- from until stop+1) {
                      maxiter=maxiter+1
                      tmp_j = temp(j0)
                      tmp_i = temp(i0)
                      if (tmp_j < tmp_i) {
                          a(k0) = tmp_j;
                          j0=j0-1;
                      }
                      else {
                          a(k0) = tmp_i;
                          i0=i0+1;
                      }
                  }

                  
              }
              i = i + 2 * m
          }
          m = 2 * m
      }

      simulator.memory.mem(1) = 1+params
      simulator.memory.mem(2) = 1+params+msize
      simulator.memory.mem(3) = msize

      for (i<-0 to msize-1) {
        simulator.memory.mem(params+1+i) = a_bk(i)
      }


      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](msize)

      for(i<- 0 to msize-1) {
        realResult(i) = simulator.memory.mem(1+params+i)
      }
      

      var pass = true
      for (i <- 0 until msize) {
        val diff = math.abs(a(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${a(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      if (pass) println("All values match!")
      else println("Some values differ.")

      println("Simulation finished.")
    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The file '$fileName' was not found.")
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace()
    }
  }


  def ATAXTest(fileName: String = "atax_arrays_output.txt", cycles: Int = 8000): Unit = {
    val cgraRows = 8
    val cgraCols = 8

    try {
      val simu = new CgraSimulator(cgraRows, cgraCols)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var params=3
      var i = 0
      var j = 0
      

      var NX = 20
      var NY = 20
      var N = 20
      var t = 0

      var x:Array[Int]=new Array[Int](N)
      var y:Array[Int]=new Array[Int](N)
      var tmp:Array[Int]=new Array[Int](N)
      var y_bk:Array[Int]=new Array[Int](N)
      var tmp_bk:Array[Int]=new Array[Int](N)
      var A:Array[Int]=new Array[Int](N*N)

      for (i<-0 to N-1) {
        x(i) = Random.nextInt(20) % 20;
        y(i) = 0;
        y_bk(i) = 0;
        tmp(i) = 0;
        tmp_bk(i) = 0;
        
        for (j<-0 to N-1) {
          A(i*N+j) = Random.nextInt(10) % 10
        }            
      }

      for (i<-0 to NX-1) {
        t = tmp(i);
        for (j<-0 to NY-1)
        {
            t = t + A(i*N+j) * x(j);
        }
        for (j<-0 to NY-1)
        {
            y(j) = y(j) + A(i*N+j) * t;
        }
        tmp(i) = t;
      } 


      simulator.memory.mem(1+N*N) = 1+params+N*N
      simulator.memory.mem(2+N*N) = 1+params+N*N+N
      simulator.memory.mem(3+N*N) = 1+params+N*N+N+N

      for (i<-0 to N-1) {
        simulator.memory.mem(params+1+i+N*N) = x(i)
        simulator.memory.mem(params+1+i+N+N*N) = y_bk(i)
        simulator.memory.mem(params+1+i+N+N+N*N) = tmp_bk(i)
      }
      for(i<- 0 to N*N-1) { 
        simulator.memory.mem(1+i) = A(i)
      }

      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](N)

      for(i<- 0 to N-1) {
        realResult(i) = simulator.memory.mem(1+params+N*N+N+i)
      }
      

      var pass = true
      for (i <- 0 until N) {
        val diff = math.abs(y(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${y(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      if (pass) println("All values match!")
      else println("Some values differ.")

      println("Simulation finished.")
    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The file '$fileName' was not found.")
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def GetTanhTest(fileName: String, cycles: Int): Unit = {
    val cgraRows = 8
    val cgraCols = 8

    try {
      val simu = new CgraSimulator(cgraRows, cgraCols)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      var simulator = new simu.CGRA(cgraRows, cgraCols, config)

      for (i<-3 to 102) {
        var f: Float = 0.1f
        if (i%10==0) {
          f = Random.nextFloat()
        } else {
          f = Random.nextFloat()*4
        }
        simulator.memory.mem(i) = java.lang.Float.floatToIntBits(f.toFloat)
      }
      for (i<-103 to 202) {
        simulator.memory.mem(i) = i-103
      }

      simulator.memory.mem(1)=3
      simulator.memory.mem(2)=103

      val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))

      // Golden result
      for (i <- 0 until 100) {
        val address = addr(i)
        val beta = A(address)
        val result =
          if (beta >= 1.0f) 1.0f
          else ((beta * beta + 19.52381f) * beta * beta + 3.704762f) * beta
        A(address) = result
      }

      

      simulator.run(cycles)

      val realResult = Array.tabulate(100) (i => 
        java.lang.Float.intBitsToFloat(simulator.memory.mem(i+3))
      )

      var pass = true
      for (i <- 0 until 100) {
        val diff = math.abs(A(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${A(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      if (pass) println("All values match!")
      else println("Some values differ.")

      println("Simulation finished.")
    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The file '$fileName' was not found.")
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def KmpTest(fileName: String = "kmpbk.ll_arrays_output.txt", cycles: Int = 1000): Unit = {
    val cgraRows = 17
    val cgraCols = 17

    try {
      val simu = new CgraSimulator(cgraRows, cgraCols)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      var PatternSize: Int = 4
      var StringSize: Int=8
      var pattern: Array[Int]=new Array[Int](PatternSize)
      var input: Array[Int]=new Array[Int](StringSize)
      var kmpNext: Array[Int]=new Array[Int](PatternSize)
      var kmpNextBK: Array[Int]=new Array[Int](PatternSize)
      var n_matches: Array[Int]=new Array[Int](1)

      var i: Int = 0
      var params: Int = 6
      ///////////////////////Init input arrays
      n_matches(0)=0
      for (i<-0 until PatternSize) {
        // feature(i) = Random.nextInt(1000) % 100
        // weight(i) = Random.nextInt(100) % 100
        // hist(i) = Random.nextInt(100) % 100
        // hist_bk(i) = hist(i)
        pattern(i) = Random.nextInt(1000) % 100
        kmpNext(i) = 1
        kmpNextBK(i)=kmpNext(i)
      }
      for (i<-0 until StringSize) {
        // feature(i) = Random.nextInt(1000) % 100
        // weight(i) = Random.nextInt(100) % 100
        // hist(i) = Random.nextInt(100) % 100
        // hist_bk(i) = hist(i)
        input(i) = Random.nextInt(1000) % 100
        
        
      }
      var start=Random.nextInt(StringSize-PatternSize-1)
      for (i<-start until start+PatternSize) {
        // feature(i) = Random.nextInt(1000) % 100
        // weight(i) = Random.nextInt(100) % 100
        // hist(i) = Random.nextInt(100) % 100
        // hist_bk(i) = hist(i)
        input(i) = pattern(i-start)
        
      }
      ///////////////// local params
      var q=0
      //////////////////main function
      // println(s"HLS c simulation start") 
      var k = 0;
      kmpNext(0)= 0;

      for (q <- 1 until PatternSize) {
          while (k > 0 && pattern(k) != pattern(q)) {
              k = kmpNext(q);
          }
          if (pattern(k) == pattern(q)) {
              k=k+1;
          }
          kmpNext(q) = k;
      }
      q=0
      for(i <- 0 to StringSize-1){
          while (q > 0 && pattern(q) != input(i)){
              q = kmpNext(q);
          }
          if (pattern(q) == input(i)){
              q=1+q;
          }
          if (q >= PatternSize){
              n_matches(0)=n_matches(0)+1;
              q = kmpNext(q-1);
          }
      }
      simulator.memory.mem(1) = 1+params
      simulator.memory.mem(2) = 1+params+PatternSize
      simulator.memory.mem(3) = 1+params+PatternSize+StringSize
      simulator.memory.mem(4) = 1+params+PatternSize+StringSize+PatternSize
      simulator.memory.mem(5)=0
      simulator.memory.mem(6)=0

      for (i<-0 until PatternSize) {
        simulator.memory.mem(params+1+i) = pattern(i)
      }
      for (i<-0 until StringSize) {
        simulator.memory.mem(params+1+i+PatternSize) = input(i)
      }
      for (i<-0 until PatternSize) {
        simulator.memory.mem(params+1+i+PatternSize+StringSize) = kmpNextBK(i)
      }
      simulator.memory.mem(simulator.memory.mem(4))=0
      simulator.run(cycles)

      var realResult:Array[Int]=new Array[Int](1)
      for (i<-0 until 1) {
        realResult(i) = simulator.memory.mem(simulator.memory.mem(4))
      }
      var p=simulator.memory.mem(5)
      var p2=simulator.memory.mem(6)
      var pass = true
      var realResult2:Array[Int]=new Array[Int](PatternSize)
      for (i <- 0 until PatternSize) {
        realResult2(i) = simulator.memory.mem(params+1+i+PatternSize+StringSize)
      }
      for (i <- 0 until PatternSize) {
        val diff = math.abs(kmpNext(i) - realResult2(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${kmpNext(i)}%f, got=${realResult2(i)}%f, diff=$diff%f")
          
          pass = false
        }
      }
      for (i <- 0 until 1) {
        val diff = math.abs(n_matches(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${n_matches(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          println(f"p is $p")
          println(f"i is $p2")
          pass = false
        }
      }
      assert(pass,"Some values differ")
      if (pass) println("All kmp values match!")

      println("Simulation finished.")

    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The file '$fileName' was not found.")
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace()
    }
  }
}
