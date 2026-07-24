package mycgratemporal

import scala.collection.mutable.{ArrayBuffer, Queue, Map, Set}
import scala.io.Source
import java.io.FileNotFoundException
import scala.util.Random
import scala.collection.mutable
// import scala.Enumeration.Value


// sbt "runMain mycgratemporal.Main" > output.log

class CgraSimulatorTemporal(val rows: Int = 8, val cols: Int = 8, DATA_WIDTH: Int = 32, tagWidth: Int = 16, numInst: Int = 32, outnum: Int = 9, fifoDepth: Int = 8, numDin: Int = 4, val if_print: Boolean = false, val if_print_config: Boolean = false) {
  
  // --- Configuration and Data Structures ---

  /** The width of the data and instructions in bits. */
  // val DATA_WIDTH: Int = 32
  val INSTRUCTION_WIDTH: Int = 64
  // val IN_FIFO_DEPTH: Int = 4

  /** Represents a single PE's state. */
  sealed trait PEState
  case object Stall extends PEState
  case object CanRun extends PEState

  //instruction type: (tagWidth-1:0) is the tag, indicates the instruction's name 
  //instruction only contains loopoutdatanew(reverse or not), philoopdata,phibranchdata, memory ops, mul, add, div, bool, compare
  //0 is loopout, 1 is reversed loopout, 2 is philoop, 3 is phibranch, 4 is mul, 5 is add, 6 is sub, 7 is eq, 8 is lt, 9 is gt, 10 is uneq, 11 is leq, 12 is geq,
  //13 is slt, 14 is sgt, 15 is sleq, 17 is sgeq, 18 is and, 19 is or, 21 is left shift, 22 is unsigned right shift, 23 is right shift, 24 is xor, 25 div, 26 sdiv
  //27 is carry-add, 28 is carry-add has cin, 29 is load, 30 is store, 31 for predicated load, 32 for predicated store no out, 33 for predicated store out,16 for store out
  //34 for half add vec, 35 for quarter add vec, 36 for half sub vec, 37 for quarter sub vec, 38 is arbitary add, 39 for mul, 40 for mulvec16, 41 for mulvec32,42 for predicated or, 
  //43 for predicated reverse or, 44 for dataTrigger immgen, 60 for urem, 61 for srem, 62 for fmult, 63 for fadd, 64 for fdiv,65 for fsub, 66 for fp2int,67 int2fp
  //67 pred fp2int,68 pred int2fp

  object InstructionType extends Enumeration {
    type InstructionType = Value

    val LOOP_OUT_DATA        = Value(0)
    val LOOP_OUT_DATA_REV    = Value(1)
    val PHI_LOOP_DATA        = Value(2)
    val PHI_BRANCH_DATA      = Value(3)
    val MUL                  = Value(4)
    val ADD32                = Value(5)
    val SUB32                = Value(6)
    val EQ                   = Value(7)
    val ULT                  = Value(8)
    val UGT                  = Value(9)
    val NEQ                  = Value(10)
    val ULE                  = Value(11)
    val UGE                  = Value(12)
    val SLT                  = Value(13)
    val SGT                  = Value(14)
    val SLE                  = Value(15)
    val STORE_OUT            = Value(16)
    val SGE                  = Value(17)
    val AND                  = Value(18)
    val OR                   = Value(19)
    val RESERVED_20          = Value(20)
    val SHL                  = Value(21)
    val SHR_LOGIC            = Value(22)
    val SHR_ARITH            = Value(23)
    val XOR                  = Value(24)
    val UDIV                  = Value(25)
    val SDIV                 = Value(26)
    val Carry_ADD_NO_CIN     = Value(27)
    val Carry_ADD_WITH_CIN   = Value(28)
    val LOAD                 = Value(29)
    val STORE                = Value(30)
    val PRED_LOAD            = Value(31)
    val PRED_STORE_NO_COUT   = Value(32)
    val PRED_STORE_WITH_COUT = Value(33)
    val VADD16               = Value(34)
    val VADD8                = Value(35)
    val VSUB16               = Value(36)
    val VSUB8                = Value(37)
    val ANYWIDTH_ADD         = Value(38)
    val MUL32                = Value(39)
    val VMUL16               = Value(40)
    val VMUL8                = Value(41)
    val PRED_OR              = Value(42)
    val PRED_OR_REV          = Value(43)
    val DATA_TRIGGERED_GEN   = Value(44)
    val NULL                 = Value(45)

    val UREM                 = Value(60)
    val SREM                 = Value(61)
    val FMUL                 = Value(62)
    val FADD                 = Value(63)
    val FDIV                 = Value(64)
    val FSUB                 = Value(65)
    val FP2INT               = Value(66)
    val INT2FP               = Value(67)
    val PRED_FP2INT          = Value(68)
    val PRED_INT2FP          = Value(69)

    def fromInt(i: Int): Option[InstructionType] = values.find(_.id == i)
  }


  case class Instruction(
    opType: InstructionType.Value,         // 8b
    immediateValue: Int,              // 32b
    opWidth: Int,          // 6b
    immSel: Int,           // 2b
    tag: Int               // 16b
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

    val numInst: Int = 16,
    var instructions: Map[Int, Instruction] = Map.empty, // key is instId
    var fsms: Map[Int, Map[InstructionType.Value, StatefulInstruction]] = Map.empty,
    
    var state: PEState = CanRun,

    var chosenInst: Option[Int] = None, // inst_id chosen by tagsolve's arbiter

    // (valid, tag, data) 
    var data_in0: DataEntry = DataEntry(false, -1, 0),
    var data_in1: DataEntry = DataEntry(false, -1, 0),
    var data_in2: DataEntry = DataEntry(false, -1, 0),
    var carry_in: DataEntry = DataEntry(false, -1, 0),

    var data_out: DataEntry = DataEntry(false, -1, 0),
    var carry_out: DataEntry = DataEntry(false, -1, 0),

    var outValid: Boolean = false,
    var dataOutReady: Boolean = true, //downstream pe is ready
    var outFire: Boolean = false, //handshaked this cycle

    // per instruction
    var loop_out_valid: Map[Int, Boolean] = Map.empty,
    var phi_loop_valid: Map[Int, Boolean] = Map.empty,
    var phi_loop_imm_end: Map[Int, Boolean] = Map.empty,
    var execute_once: Map[Int, Boolean] = Map.empty,
    var route_data_valid: Map[Int, Boolean] = Map.empty,
    var route_pred_valid: Map[Int, Boolean] = Map.empty,


    // -------- Memory requests per instruction --------
    var currentLoadReq: Map[Int, Option[LoadReq]] = Map.empty,
    var currentStoreReq: Map[Int, Option[StoreReq]] = Map.empty,
    var loadBook: Map[Int, LoadBook] = Map.empty, 
    var storeBook: Map[Int, StoreBook] = Map.empty, 

    // phi_loop_state
    var current_state: Map[Int, PhiLoopState.Value] = Map.empty,
    var philoopstate: Map[Int, Int] = Map.empty, // (0=Init,1=Loop)
    var backup_state: Map[Int, PhiLoopState.Value] = Map.empty,
    var drop_data: Map[Int, Boolean] = Map.empty,

    // reg
    var reg_data: Int = 1,
    var reg_valid: Array[Int] = Array.fill(4)(0),
    var reg_tag: Int = 0

  )

  object LoadState extends Enumeration {
    type LoadState = Value
    val Idle, Issue, Out = Value
  }
  import LoadState._

  case class LoadBook (
    state: LoadState = LoadState.Idle,
    dataBuf: Option[Int] = None //data loaded from memory      
  )

  object StoreState extends Enumeration { 
    type StoreState = Value
    val Idle, Issue, Out = Value 
  }
  import StoreState._

  case class StoreBook (
    state: StoreState = StoreState.Idle
  )

  /**
   * A class to hold the parsed configuration from the input file.
   *
   * @param peInstArray Map of PE ID to its 64-bit instruction represented as a bit array.
   * @param peIdToOutSetD0  Map of PE ID to a bit array representing destinations for output data 0.
   * @param peIdToOutSetD1  Map of PE ID to a bit array representing destinations for output data 1.
   * @param peIdToOutSetP0  Map of PE ID to a bit array representing destinations for output predicate 0.
   * @param peIdToOutSetP1  Map of PE ID to a bit array representing destinations for output predicate 1.
   */
  case class CgraConfiguration(
    peInstArray: Map[Int, Array[Array[Int]]],
    peIdToOutSetD0: Map[Int, Array[Seq[Int]]],
    peIdToOutSetD1: Map[Int, Array[Seq[Int]]],
    peIdToOutSetD2: Map[Int, Array[Seq[Int]]],
    peIdToOutSetC: Map[Int, Array[Seq[Int]]],
    peDataNum: Map[Int, Array[Array[Int]]],
    peRedistTag: Map[Int, Array[Array[Array[Int]]]],
    peRedistForward: Map[Int, Array[Array[Array[Int]]]],
    peRegTag: Map[Int, Array[Int]],
    peRegInputs: Map[Int, Array[Int]],
    peRegValid: Map[Int, Int]
  )

  import scala.collection.mutable.{Queue, ArrayBuffer}

  // Load/Store requests with remaining cycles
  case class LoadReq(addr: Int, var cyclesLeft: Int, var data: Int = 0, var valid: Boolean = false, var issue: Boolean = false)
  case class StoreReq(addr: Int, data: Int, var cyclesLeft: Int, var valid: Boolean = false, var issue: Boolean = false)

  import scala.util.Random

  class Memory(
      val size: Int = 65536,
      val readPorts: Int = 1,
      val writePorts: Int = 1,
      val accessLatency: Int = 1
  ) {
    // private 
    val mem = Array.fill(size)(0)
    

    private val loadQueue = Queue[LoadReq]()
    private val storeQueue = Queue[StoreReq]()

    private var issuedLoadsThisCycle  = 0
    private var issuedStoresThisCycle = 0

    private def resetCycle(): Unit = {
      issuedLoadsThisCycle = 0
      issuedStoresThisCycle = 0
    }


    /** PE issues a load request */
    def load(addr: Int): LoadReq = {
      require(addr >= 0 && addr < size)
      val req = LoadReq(addr, accessLatency)
      if (issuedLoadsThisCycle < readPorts) {
        req.issue = true
        issuedLoadsThisCycle += 1
      }
      loadQueue.enqueue(req)
      req
    }

    /** PE issues a store request */
    def store(addr: Int, data: Int): StoreReq = {
      require(addr >= 0 && addr < size)
      val req = StoreReq(addr, data, accessLatency)
      if (issuedStoresThisCycle < writePorts) {
        req.issue = true
        issuedStoresThisCycle += 1
      }
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

      resetCycle()
    }
  }

  def toMutableMap[A](arr: Array[A]): mutable.Map[Int, A] = {
    mutable.Map.from(arr.zipWithIndex.map { case (v, i) => i -> v })
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

    val destinations = Set.empty[Int]

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

  def getOrderedDestinations9(sourceId: Int, rows: Int, cols: Int): List[Int] = {
    val sourceRow = sourceId / cols
    val sourceCol = sourceId % cols

    val destinations = Set.empty[Int]

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

    // Ensure sourceId is first, and rest are sorted excluding sourceId
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
    // Bitmasks for each field
    val m: Long = if (DATA_WIDTH <= 0 || DATA_WIDTH > 64) 0L else (1L << DATA_WIDTH) - 1
    val immBitsRaw     = ((rawInst >>> DATA_WIDTH) & m).toInt  // 32b
    val reversedImm = reverseBitsDATA_WIDTH(immBitsRaw)
    val immediateValue = immBitsRaw.toInt
    // if (if_print) {
    //   print("imm:")
    //   println(immediateValue)
    //   println("imm: " + java.lang.Float.intBitsToFloat(immediateValue))
    // }
    val opWidth  = ((rawInst >>> 26) & 0x3F).toInt         // 6b
    val immSel   = ((rawInst >>> 24) & 0x3).toInt          // 2b
    val instType = ((rawInst >>> 16) & 0xFF).toInt         // 8b
    val tag      = (rawInst & 0xFFFF).toInt                // 16b

    var opType = InstructionType.fromInt(instType).getOrElse(InstructionType.NULL)
    if (instType == 0 && immSel == 0 && opWidth == 0 && tag == 0) {
      opType = InstructionType.NULL
    }
    
    Instruction(opType, immediateValue, opWidth, immSel, tag)
  }

  def reverseBitsDATA_WIDTH(x: Long): Long = {
    var in  = x
    var out = 0L
    for (i <- 0 until DATA_WIDTH) {
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

  def parseBitArrayToInt(bits: Array[Int]): Int = {
    var result: Int = 0
    for (i <- bits.indices) {
      if (bits(i) == 1) {
        result |= (1 << i)  // LSB at index 0
      }
    }
    result
  }

  def intToBitArray(value: Int, width: Int): Array[Int] = {
    Array.tabulate(width)(i => if ((value & (1 << i)) != 0) 1 else 0)
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
  def readConfigFromFile(fileContent: String, rows: Int, cols: Int, tagWidth: Int=16, numInst: Int=16, outnum: Int=9): CgraConfiguration = {
    val lines = fileContent.split("\n").map(_.trim).filterNot(_.isEmpty)

    // parse instarray: PEInstArray(pe)(inst)
    def parsePEInst(): Array[Array[Array[Int]]] = {

      val peBuffer = ArrayBuffer[ArrayBuffer[Array[Int]]]()
      var currentPE: ArrayBuffer[Array[Int]] = null

      for (line <- lines) {
        val trimmed = line.trim
        if (trimmed.startsWith("remuPEInst(") && trimmed.contains("Instructions:") && !trimmed.contains("):")) {
          currentPE = ArrayBuffer[Array[Int]]()
          peBuffer += currentPE
        } else if (trimmed.startsWith("remuPEInst(") && trimmed.contains("):")) {
          val parts = trimmed.split(":")
          if (parts.length == 2) {
            val nums = parts(1).split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt)
            currentPE += nums
          }
        }
      }

      peBuffer.map(pe => pe.toArray).toArray
    }

    val peInstArray = parsePEInst()

    def printPEInst(peInst: Array[Array[Array[Int]]]): Unit = {
      println("==== remuPEInst ====")
      for (pe <- peInst.indices) {
        println(s"PE($pe):")
        for (inst <- peInst(pe).indices) {
          val instBits = peInst(pe)(inst).mkString(", ")
          println(s"  Inst($inst): $instBits")
        }
      }
      println()
    }

    if(if_print_config) printPEInst(peInstArray)

    /*-------------------------------------------------------------------------------*/
    // parse peout: PEOut(pe)(d)(inst)(port)
    def parseRemuPEOut(): Array[Array[Array[Array[Int]]]] = {
      val data = ArrayBuffer[ArrayBuffer[ArrayBuffer[Array[Int]]]]()
      var currentPE: ArrayBuffer[ArrayBuffer[Array[Int]]] = null
      var currentD = 0

      for (line <- lines) {
        val trimmed = line.trim

        if (trimmed.startsWith("remuPEOut(") && trimmed.contains("Routing D")) {
          val peIndex = "\\((\\d+)\\)".r.findFirstMatchIn(trimmed).get.group(1).toInt
          val dStr = "D(\\d+)".r.findFirstMatchIn(trimmed).get.group(1).toInt
          currentD = dStr

          while (data.size <= peIndex) data += ArrayBuffer()
          currentPE = data(peIndex)

          while (currentPE.size <= currentD) currentPE += ArrayBuffer()
        }
        else if (trimmed.startsWith("remuPEOut Routing D")) {
          val parts = trimmed.split(":")
          if (parts.length == 2) {
            val nums = parts(1).split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt)
            currentPE(currentD) += nums
          }
        }
      }

      val numPE = data.length
      val numD = data.map(_.length).max
      // val numInst = data.flatMap(_.map(_.length)).max
      // val numOut = data.flatMap(_.flatMap(_.map(_.length))).max

      val remuPEOut = Array.ofDim[Int](numPE, numD, numInst, outnum)

      for (pe <- data.indices) {
        for (d <- data(pe).indices) {
          for (inst <- data(pe)(d).indices) {
            for (port <- data(pe)(d)(inst).indices) {
              remuPEOut(pe)(d)(inst)(port) = data(pe)(d)(inst)(port)
            }
          }
        }
      }

      remuPEOut
    }
    val peOut = parseRemuPEOut()

    def printRemuPEOut(remu: Array[Array[Array[Array[Int]]]]): Unit = {
      println("==== remuPEOut ====")
      for (pe <- remu.indices) {
        println(s"PE($pe):")
        for (d <- remu(pe).indices) {
          println(s"  Routing D$d:")
          for (inst <- remu(pe)(d).indices) {
            val outStr = remu(pe)(d)(inst).mkString(", ")
            println(s"    Inst($inst): $outStr")
          }
        }
      }
      println()
    }

    if(if_print_config) printRemuPEOut(peOut)


    /*-------------------------------------------------------------------------------*/
    // datanum: remuPEData(pe)(inst)(dataIndex)
    def parseRemuPEData(): Array[Array[Array[Int]]] = {
      val data = ArrayBuffer[ArrayBuffer[Array[Int]]]()
      var currentPE: ArrayBuffer[Array[Int]] = null

      for (line <- lines) {
        val trimmed = line.trim

        if (trimmed.startsWith("remuPEData(") && trimmed.endsWith(":")) {
          val peIndex = "\\((\\d+)\\)".r.findFirstMatchIn(trimmed).get.group(1).toInt
          while (data.size <= peIndex) data += ArrayBuffer()
          currentPE = data(peIndex)
        }
        else if (trimmed.startsWith("remuPEData(")) {
          val parts = trimmed.split(":")
          if (parts.length == 2) {
            val nums = parts(1).split(",").map(_.trim).filter(_.nonEmpty).map(_.toInt)
            currentPE += nums
          }
        }
      }

      val numPE = data.length
      // val numInst = data.map(_.length).max
      val numVec = data.flatMap(_.map(_.length)).max

      val remuPEData = Array.ofDim[Int](numPE, numInst, numVec)

      for (pe <- data.indices) {
        for (inst <- data(pe).indices) {
          for (i <- data(pe)(inst).indices) {
            remuPEData(pe)(inst)(i) = data(pe)(inst)(i)
          }
        }
      }

      remuPEData
    }
    val peDataNum = parseRemuPEData()

    def printRemuPEData(data: Array[Array[Array[Int]]]): Unit = {
      println("==== remuPEData ====")
      for (pe <- data.indices) {
        println(s"PE($pe):")
        for (inst <- data(pe).indices) {
          val values = data(pe)(inst).mkString(", ")
          println(s"  Inst($inst): $values")
        }
      }
      println()
    }

    if(if_print_config) printRemuPEData(peDataNum)

    /*-------------------------------------------------------------------------------*/
    // Redistribute: PERedist(pe)(inst)(data)(0 = tags / 1 = forward)(tagWidth)
    def parseRemuRedist(): (Array[Array[Array[Array[Int]]]], Array[Array[Array[Array[Int]]]]) = {

      val data = ArrayBuffer[ArrayBuffer[ArrayBuffer[Array[Int]]]]()
      var currentPE: ArrayBuffer[ArrayBuffer[Array[Int]]] = null
      var currentInst: ArrayBuffer[Array[Int]] = null

      val peInstPattern = """PE\((\d+)\) Inst\((\d+)\) Data(\d):""".r

      var i = 0
      while (i < lines.length) {
        val trimmed = lines(i).trim

        trimmed match {
          case peInstPattern(peStr, instStr, dataStr) =>
            val peIdx = peStr.toInt
            val instIdx = instStr.toInt
            val dataIdx = dataStr.toInt

            while (data.size <= peIdx) data += ArrayBuffer()
            currentPE = data(peIdx)

            while (currentPE.size <= instIdx) currentPE += ArrayBuffer()
            currentInst = currentPE(instIdx)

            while (currentInst.size <= dataIdx) currentInst += Array.ofDim[Int](tagWidth + numInst)

            if (i + 1 < lines.length && lines(i + 1).trim.startsWith("remuRedist Tags")) {
              val tags = lines(i + 1).trim.split(":")(1).trim.split(",").map(_.trim.toInt)
              Array.copy(tags, 0, currentInst(dataIdx), 0, tagWidth)
            }

            if (i + 2 < lines.length && lines(i + 2).trim.startsWith("remuRedist Forward")) {
              val fwd = lines(i + 2).trim.split(":")(1).trim.split(",").map(_.trim.toInt)
              //println(fwd)
              Array.copy(fwd, 0, currentInst(dataIdx), tagWidth, numInst)
            }

            i += 2
          case _ =>
        }

        i += 1
      }

      val numPE = data.size
      // val numInst = data.map(_.size).max
      println(numInst," is the numInst")
      val numData = 4 // Data0~Data3
      val remuRedist = Array.ofDim[Int](numPE, numInst, numData, 2, tagWidth)

      val PERedistTag     = Array.ofDim[Int](numPE, numInst, numData, tagWidth)
      val PERedistForward = Array.ofDim[Int](numPE, numInst, numData, numInst)

      for (pe <- data.indices) {
        for (inst <- data(pe).indices) {
          for (d <- data(pe)(inst).indices) {
            val arr = data(pe)(inst)(d)
            for (t <- 0 until tagWidth) {
              PERedistTag(pe)(inst)(d)(t)     = arr(t)
              //PERedistForward(pe)(inst)(d)(t) = arr(tagWidth + t)
            }
            for (t <- 0 until numInst) {
             // PERedistTag(pe)(inst)(d)(t)     = arr(t)
              PERedistForward(pe)(inst)(d)(t) = arr(tagWidth + t)
            }
          }
        }
      }

      (PERedistTag, PERedistForward)
    }

    val (peRedistTag, peRedistForward) = parseRemuRedist()

    def printRemuRedist(
      tags: Array[Array[Array[Array[Int]]]],
      fwd: Array[Array[Array[Array[Int]]]]
    ): Unit = {
      println("==== remuRedist ====")
      for (pe <- tags.indices) {
        println(s"PE($pe):")
        for (inst <- tags(pe).indices) {
          println(s"  Inst($inst):")
          for (d <- tags(pe)(inst).indices) {
            val tagStr = tags(pe)(inst)(d).mkString(", ")
            val fwdStr = fwd(pe)(inst)(d).mkString(", ")
            println(s"    Data$d:")
            println(s"      Tags   ($tagStr)")
            println(s"      Forward($fwdStr)")
          }
        }
      }
      println()
    }

    if(if_print_config) printRemuRedist(peRedistTag, peRedistForward)

    /*-------------------------------------------------------------------------------*/
    //RegV: PERegTag(pe)(tagbits), PERegInputs(pe)(d0-d3)
    def parseRemuRegV(): (Array[Array[Int]], Array[Array[Int]]) = {

      val regIndices = lines.filter(_.startsWith("remuRegV(")).map { line =>
        line.substring(line.indexOf("(") + 1, line.indexOf(")")).toInt
      }
      val regNum = if (regIndices.nonEmpty) regIndices.max + 1 else 0

      val remuRegV_tags   = Array.ofDim[Int](regNum, tagWidth)
      val remuRegV_inputs = Array.ofDim[Int](regNum, 4)

      var curReg = -1
      for (line <- lines) {
        if (line.startsWith("remuRegV(")) {
          curReg = line.substring(line.indexOf("(") + 1, line.indexOf(")")).toInt
        }
        else if (line.startsWith("remuRegV Tags")) {
          val nums = line.split(":")(1).split(",").map(_.trim.toInt)
          for (i <- nums.indices) remuRegV_tags(curReg)(i) = nums(i)
        }
        else if (line.startsWith("remuRegV Inputs")) {
          val nums = line.split(":")(1).split(",").map(_.trim.toInt)
          for (i <- nums.indices) remuRegV_inputs(curReg)(i) = nums(i)
        }
      }

      (remuRegV_tags, remuRegV_inputs)
    }

    val (peRegTag, peRegInputs) = parseRemuRegV()

    def printRemuRegV(tags: Array[Array[Int]], inputs: Array[Array[Int]]): Unit = {
      println("==== remuRegV ====")
      for (reg <- tags.indices) {
        println(s"remuRegV($reg):")

        val tagStr = tags(reg).mkString(", ")
        println(s"  Tags:   $tagStr")

        val inputStr = inputs(reg).mkString(", ")
        println(s"  Inputs: $inputStr")
      }
      println()
    }

    if (if_print_config) printRemuRegV(peRegTag, peRegInputs)

    /*-------------------------------------------------------------------------------*/
    //RegValid which PE's reg send data: PERegValid(pe)
    def parseRemuRegValid(): Array[Int] = {
      val buffer = ArrayBuffer[Int]()

      var inSection = false
      for (line <- lines.map(_.trim)) {
        if (line.startsWith("remuRegValid:")) {
          inSection = true
        } else if (inSection && line.startsWith("remuRegValid(")) {
          val parts = line.split(":").map(_.trim)
          if (parts.length == 2) {
            buffer.append(parts(1).toInt)
          }
        } else if (inSection && line.isEmpty) {
          inSection = false
        }
      }

      buffer.toArray
    }
    val peRegValid = parseRemuRegValid()

    def printRemuRegValid(arr: Array[Int]): Unit = {
      println("==== remuRegValid ====")
      for ((value, idx) <- arr.zipWithIndex) {
        println(s"remuRegValid($idx) = $value")
      }
      println()
    }

    if (if_print_config) {
      printRemuRegValid(peRegValid)
    }

    /*-------------------------------------------------------------------------------*/
    // RegTag: PERegTag(pe)(tagbits)
    def parseRemuRegTags(): Array[Array[Int]] = {
      val regIndices = lines.filter(_.startsWith("remuRegTags(")).map { line =>
        line.substring(line.indexOf("(") + 1, line.indexOf(")")).toInt
      }
      val regNum = if (regIndices.nonEmpty) regIndices.max + 1 else 0

      val remuRegTags = Array.ofDim[Int](regNum, tagWidth)

      for (line <- lines) {
        if (line.startsWith("remuRegTags(")) {
          val idx = line.substring(line.indexOf("(") + 1, line.indexOf(")")).toInt
          val nums = line.split(":")(1).split(",").map(_.trim.toInt)
          for (i <- nums.indices) remuRegTags(idx)(i) = nums(i)
        }
      }

      remuRegTags
    }


    def buildOutSet(peOut: Array[Array[Array[Array[Int]]]],
                d: Int, rows: Int, cols: Int, ifPrint: Boolean = true): Map[Int, Array[Seq[Int]]] = {
      val result = Map[Int, Array[Seq[Int]]]()
      for (peId <- peOut.indices) {
        if (d < peOut(peId).length) {
          val insts = peOut(peId)(d)
          var destinationList = getOrderedDestinations9(peId, rows, cols)
          if(outnum==rows*cols)
            destinationList = getOrderedDestinations(peId, rows, cols)
          
          if (ifPrint) println(s"PE($peId) D$d destination list: $destinationList")

          val instOuts: Array[Seq[Int]] = insts.map { bitVec =>
            bitVec.zipWithIndex
              .filter(_._1 == 1)
              .map { case (_, idx) => destinationList.lift(idx).getOrElse(-1) }
              .filter(_ != -1)
              .toSeq
          }

          result(peId) = instOuts
          if (ifPrint) {
            instOuts.zipWithIndex.foreach { case (outs, instId) =>
              println(s"  Inst($instId) -> { ${outs.mkString(", ")} }")
            }
          }
        }
      }
      result.to(mutable.Map)
    }

    def printPeIdToOutSet(
        label: String,
        peIdToOutSet: Map[Int, Array[Seq[Int]]]
    ): Unit = {
      println(s"==== peIdToOutSet$label ====")
      for ((pe, instArray) <- peIdToOutSet.toSeq.sortBy(_._1)) {
        println(s"PE($pe):")
        for (inst <- instArray.indices) {
          val dests = instArray(inst)
          if (dests.nonEmpty) {
            println(s"  Inst($inst) -> { ${dests.mkString(", ")} }")
          } else {
            println(s"  Inst($inst) -> {  }")
          }
        }
      }
      println()
    }


    val peIdToOutSetD0 = buildOutSet(peOut, 0, rows, cols, ifPrint = if_print_config)
    val peIdToOutSetD1 = buildOutSet(peOut, 1, rows, cols, ifPrint = if_print_config)
    val peIdToOutSetD2 = buildOutSet(peOut, 2, rows, cols, ifPrint = if_print_config)
    val peIdToOutSetC  = buildOutSet(peOut, 3, rows, cols, ifPrint = if_print_config)

    if (if_print_config) printPeIdToOutSet("D0", peIdToOutSetD0)
    if (if_print_config) printPeIdToOutSet("D1", peIdToOutSetD1)
    if (if_print_config) printPeIdToOutSet("D2", peIdToOutSetD2)
    if (if_print_config) printPeIdToOutSet("C",  peIdToOutSetC)

    CgraConfiguration(
      toMutableMap(peInstArray),
      peIdToOutSetD0,
      peIdToOutSetD1,
      peIdToOutSetD2,
      peIdToOutSetC,
      toMutableMap(peDataNum),
      toMutableMap(peRedistTag),
      toMutableMap(peRedistForward),
      toMutableMap(peRegTag),
      toMutableMap(peRegInputs),
      toMutableMap(peRegValid)
    )
  }


  // ---------- FSM framework ----------
  case class FSMResult(dataOut: Option[Int] = None, predOut: Option[Int] = None, drop: Boolean = false)

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
            FSMResult(drop = true)
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
      if (predIn==1 && predIn_valid && input0_valid) FSMResult(dataOut = Some(input0), predOut = Some(input0)) else FSMResult(drop = true)
    }
  }



  // ---------- FSM factory ----------
  def createFSMs(): Map[InstructionType.Value, StatefulInstruction] = {
    Map(
      InstructionType.PHI_LOOP_DATA -> new PhiLoopFSM(),
      InstructionType.LOOP_OUT_DATA -> new LoopoutFSM(),
      InstructionType.LOOP_OUT_DATA_REV -> new LoopoutFSM()
    )
  }

  // class RoundRobin[T](outNum: Int) {
  //   private var priorityOrder: List[Int] = (0 until outNum).toList

  //   def arbitrate(in: Array[Option[T]], isfired: Boolean = true): Option[(Int, T)] = {
  //     val chosenIdxOpt = priorityOrder.find(i => in(i).isDefined)

  //     chosenIdxOpt match {
  //       case Some(idx) =>
  //         val data = in(idx).get
  //         if (!isfired)
  //           priorityOrder = priorityOrder.filterNot(_ == idx) :+ idx
  //         Some(idx -> data)
  //       case None =>
  //         None
  //     }
  //   }

  //   def lowerPriority(idx: Int): Unit = {
  //     if (priorityOrder.contains(idx)) {
  //       priorityOrder = priorityOrder.filterNot(_ == idx) :+ idx
  //     }
  //   }

  //   def getPriority: List[Int] = priorityOrder
  // }

  // import scala.collection.mutable.{ArrayBuffer, Queue}

  // ---------------- RoundRobin Arbiter ----------------
  class RoundRobin[T](outNum: Int) {
    private var priorityOrder: List[Int] = (0 until outNum).toList

    def arbitrate(validMask: Array[Boolean]): Option[Int] = {
      val chosenIdxOpt = priorityOrder.find(i => validMask(i))
      chosenIdxOpt match {
        case Some(idx) =>
          // priorityOrder = priorityOrder.filterNot(_ == idx) :+ idx
          Some(idx)
        case None => None
      }
    }

    def lowerPriority(idx: Int): Unit = {
      if (priorityOrder.contains(idx)) {
        priorityOrder = priorityOrder.filterNot(_ == idx) :+ idx
      }
    }

    def getPriority: List[Int] = priorityOrder
  }

  case class DataEntry(valid: Boolean, tag: Int, data: Int)

  // ---------------- Tagsolve Module ----------------
  case class TagSolve(tagsolveid: Int, numInst: Int = 16) {
    var instructions: Map[Int, Instruction] = Map.empty

    // Input channels
    val inD0  = Array.fill(numInst)(Option.empty[DataEntry])
    val inD1  = Array.fill(numInst)(Option.empty[DataEntry])
    val inD2  = Array.fill(numInst)(Option.empty[DataEntry])
    val inCin = Array.fill(numInst)(Option.empty[DataEntry])

    val inD0_bk  = Array.fill(numInst)(Option.empty[DataEntry])
    val inD1_bk  = Array.fill(numInst)(Option.empty[DataEntry])
    val inD2_bk  = Array.fill(numInst)(Option.empty[DataEntry])
    val inCin_bk = Array.fill(numInst)(Option.empty[DataEntry])

    // need which inputs
    val dataNum = Array.fill(numInst)(0)

    // phi loop state
    val phiLoop = Array.fill(numInst)(0)

    // Round Robin Arbiter
    private val rr = new RoundRobin[Int](numInst)

    var tagsolve_mask: Int = 0

    def backupInputs(): Unit = {
      for (i <- 0 until numInst) {
        inD0_bk(i)  = inD0(i)
        inD1_bk(i)  = inD1(i)
        inD2_bk(i)  = inD2(i)
        inCin_bk(i) = inCin(i)
      }
    }

    def updateInputs(): Unit = {
      for (i <- 0 until numInst) {
        inD0(i)  = inD0_bk(i)
        inD1(i)  = inD1_bk(i)
        inD2(i)  = inD2_bk(i)
        inCin(i) = inCin_bk(i)
      }
    }


    def printInputs(): Unit = {
      val numInst = inD0.length
      for (i <- 0 until numInst) {
        
        val s0 = inD0(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")
        val s1 = inD1(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")
        val s2 = inD2(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")
        val sc = inCin(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")

        if (!(s0 == "empty" && s1 == "empty" && s2 == "empty" && sc == "empty")) {
          val dec = decodeDataNum(dataNum(i), phiLoop(i), i)
          val decBin = String.format("%4s", dec.toBinaryString).replace(' ', '0')
          println(s"Tagsolve ($tagsolveid) Inst($i) (${instructions(i)}) datanum=$decBin: inD0=$s0, inD1=$s1, inD2=$s2, inCin=$sc, ready=${isReady(i)}")
        }
      }
    }

    def printbkInputs(): Unit = {
      val numInst = inD0.length
      for (i <- 0 until numInst) {
        val s0 = inD0_bk(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")
        val s1 = inD1_bk(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")
        val s2 = inD2_bk(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")
        val sc = inCin_bk(i).map(e => s"(data=${e.data}, tag=${e.tag}, valid=${e.valid})").getOrElse("empty")

        if (!(s0 == "empty" && s1 == "empty" && s2 == "empty" && sc == "empty")) {
          val dec = decodeDataNum(dataNum(i), phiLoop(i), i)
          val decBin = String.format("%4s", dec.toBinaryString).replace(' ', '0')
          println(s"Tagsolve ($tagsolveid) Inst($i) (${instructions(i)}) datanum=$decBin: inD0=$s0, inD1=$s1, inD2=$s2, inCin=$sc, ready=${isReady(i)}")
        }
      }
    }

    // 0 is d0, 1 is  d0 and d1, 2 is d0,d1,d2, 3 is d0, cin, 4 is d0, d1, cin, 5 is d1, cin, 6 is d1, 7 is d0, d2, 8 is d1, d2, 9 is special for philoop, 10 is special for phibranch
    def decodeDataNum(dn: Int, phi: Int, instId: Int): Int = {
      dn match {
        case 0  => 0x8   // 0b1000
        case 1  => 0xC   // 0b1100
        case 2  => 0xE   // 0b1110
        case 3  => 0x9   // 0b1001
        case 4  => 0xD   // 0b1101
        case 5  => 0x5   // 0b0101
        case 6  => 0x4   // 0b0100
        case 7  => 0xA   // 0b1010
        case 8  => 0x6   // 0b0110
        case 9  => if (phi == 0) 0x8 else 0x6  // 0b1000 / 0b0110
        case 10 =>
          val imms = instructions(instId).immSel
          inD2(instId) match {
            case Some(entry) if entry.valid && entry.data == 1 => 0xA // 0b1010, d0 + d2
            case Some(entry) if entry.valid && entry.data == 0 => 0x6 // 0b0110, d1 + d2
            case _ => 0xF  // 0b1111 fallback: need all
          }
        case _  => 0x0
      }
    }



    def isReady(instId: Int): Boolean = {
      val mask = decodeDataNum(dataNum(instId), phiLoop(instId), instId)
      val imms = instructions(instId).immSel
      val d0v = if (((mask >> 3) & 1) == 1 && imms == 1) {true} else inD0(instId).exists(_.valid)
      val d1v = if (((mask >> 2) & 1) == 1 && imms == 2) {true} else inD1(instId).exists(_.valid)
      val d2v = inD2(instId).exists(_.valid)
      val cv  = inCin(instId).exists(_.valid)

      val inputs = Seq(d0v, d1v, d2v, cv)
      val needed = (0 until 4).map(i => ((mask >> (3 - i)) & 1) == 1) 
      // print(s"[isReady] Inst($instId):")
      // print(s"  datanum=${dataNum(instId)} mask(bin)=${mask.toBinaryString}")
      // print(s"  needed(d0,d1,d2,cin)=${needed.mkString(",")}")
      // print(s"  inputs(d0,d1,d2,cin)=${inputs.mkString(",")}")
      // println(s"  => ready=${needed.zip(inputs).forall { case (need, has) => !need || has }}")
      needed.zip(inputs).forall { case (need, has) => !need || has }
    }

    // ==== Main Step ====
    def step(): Option[(Int, Seq[DataEntry], Int)] = {
      val readyMask = Array.tabulate(numInst)(i => isReady(i))
      tagsolve_mask = 0
      // if()
      rr.arbitrate(readyMask) match {
        case Some(instId) =>
          val mask = decodeDataNum(dataNum(instId), phiLoop(instId), instId)

          val emptyEntry = DataEntry(false, -1, 0)
          val imms = instructions(instId).immSel

          if (((mask >> 3) & 1) == 1 && imms == 1) {
            inD0(instId) = Some(DataEntry(true, 1, instructions(instId).immediateValue))
          }
          val d0 = if (((mask >> 3) & 1) == 1) inD0(instId).getOrElse(emptyEntry) else emptyEntry

          if (((mask >> 2) & 1) == 1 && imms == 2) {
            inD1(instId) = Some(DataEntry(true, 1, instructions(instId).immediateValue))
          }
          val d1 = if (((mask >> 2) & 1) == 1) inD1(instId).getOrElse(emptyEntry) else emptyEntry

          val d2 = if (((mask >> 1) & 1) == 1) inD2(instId).getOrElse(emptyEntry) else emptyEntry
          val c  = if (((mask >> 0) & 1) == 1) inCin(instId).getOrElse(emptyEntry) else emptyEntry

          val outputs: Seq[DataEntry] = Seq(d0, d1, d2, c)

          Some((instId, outputs, mask))

        case None => None
      }
    }

    def step(currentLoadReq: Map[Int, Option[LoadReq]], currentStoreReq: Map[Int, Option[StoreReq]]): Option[(Int, Seq[DataEntry], Int)] = {
      var readyMask = Array.tabulate(numInst)(i => isReady(i))
      tagsolve_mask = 0
      var hasLS=0
      var pointer=0
      for(i<-0 until readyMask.length){
        if(currentLoadReq.contains(i))
          currentLoadReq(i) match {
            case Some(req) =>
              hasLS=1
              pointer=i
            case _ => 
          }
      }
      
      for(i<-0 until readyMask.length){
        if(currentStoreReq.contains(i))
          currentStoreReq(i) match {
            case Some(req) =>
              hasLS=1
              pointer=i
            case _ => 
          }
      }
      if(hasLS==1){
        for(i<-0 until readyMask.length){
          if(i!=pointer){
            readyMask(i)=false
          }
          else{
            assert(readyMask(i)==true)
          }
        }
      }
      rr.arbitrate(readyMask) match {
        case Some(instId) =>
          val mask = decodeDataNum(dataNum(instId), phiLoop(instId), instId)

          val emptyEntry = DataEntry(false, -1, 0)
          val imms = instructions(instId).immSel

          if (((mask >> 3) & 1) == 1 && imms == 1) {
            inD0(instId) = Some(DataEntry(true, 1, instructions(instId).immediateValue))
          }
          val d0 = if (((mask >> 3) & 1) == 1) inD0(instId).getOrElse(emptyEntry) else emptyEntry

          if (((mask >> 2) & 1) == 1 && imms == 2) {
            inD1(instId) = Some(DataEntry(true, 1, instructions(instId).immediateValue))
          }
          val d1 = if (((mask >> 2) & 1) == 1) inD1(instId).getOrElse(emptyEntry) else emptyEntry

          val d2 = if (((mask >> 1) & 1) == 1) inD2(instId).getOrElse(emptyEntry) else emptyEntry
          val c  = if (((mask >> 0) & 1) == 1) inCin(instId).getOrElse(emptyEntry) else emptyEntry

          val outputs: Seq[DataEntry] = Seq(d0, d1, d2, c)

          Some((instId, outputs, mask))

        case None => None
      }
    }

    def clearTagsolveInputs(instId: Int): Unit = {
      if (((tagsolve_mask >> 3) & 1) == 1) inD0_bk(instId) = None
      if (((tagsolve_mask >> 2) & 1) == 1) inD1_bk(instId) = None
      if (((tagsolve_mask >> 1) & 1) == 1) inD2_bk(instId) = None
      if (((tagsolve_mask >> 0) & 1) == 1) inCin_bk(instId) = None
    }


    // ==== Input interface ====
    def pushData(instId: Int, port: Int, tag: Int, data: Int): Boolean = {
      val entry = DataEntry(true, tag, data)
      port match {
        case 0 =>
          if (inD0(instId).exists(_.valid)) false
          else { 
            inD0_bk(instId) = Some(entry); 
            if (if_print) println(s"pushed data($data) into tagsolve inst$instId d$port")
            true 
          }

        case 1 =>
          if (inD1(instId).exists(_.valid)) false
          else { 
            inD1_bk(instId) = Some(entry); 
            if (if_print) println(s"pushed data($data) into tagsolve inst$instId d$port")
            true 
          }

        case 2 =>
          if (inD2(instId).exists(_.valid)) false
          else { 
            inD2_bk(instId) = Some(entry); 
            if (if_print) println(s"pushed data($data) into tagsolve inst$instId d$port")
            true 
          }

        case 3 =>
          if (inCin(instId).exists(_.valid)) false
          else { 
            inCin_bk(instId) = Some(entry); 
            if (if_print) println(s"pushed data($data) into tagsolve inst$instId d$port")
            true 
          }

        case _ => false
      }
    }


    // ==== Control interface ====
    def lowerPriority(instId: Int): Unit = rr.lowerPriority(instId)
    def getPriority: List[Int] = rr.getPriority
  }

  case class ReviseEntry(dataBit: Int, tag: Int)
  
  case class RemuInputNew(val din: Int = 0, val subInstNum: Int = 16, fifoDepth: Int = 8, outnum: Int = 9) {
    val table = Array.fill(subInstNum)(ReviseEntry(0, -1))
    val channels: Array[Option[DataEntry]] = Array.fill(outnum+1)(None)
    val fifos: Array[Queue[(Int, Int)]] = Array.fill(subInstNum)(Queue[(Int, Int)]())

    val channels_bk: Array[Option[DataEntry]] = Array.fill(outnum+1)(None)
    val fifos_bk: Array[Queue[(Int, Int)]] = Array.fill(subInstNum)(Queue[(Int, Int)]())

    // transferState(row)(col) = 0/1
    val transferState: Array[Array[Int]] = Array.fill(subInstNum, subInstNum)(0)

    val transferState_bk: Array[Array[Int]] = Array.fill(subInstNum, subInstNum)(0)

    val fifo_out_bool: Array[Boolean] = Array.fill(subInstNum)(false)

    private val arb9to1  = new RoundRobin(outnum+1)
    private val arb16to1 = new RoundRobin(subInstNum)

    var choosenChannel=0

    case class Pending(var valid: Boolean = false, var data: Int = 0, var tag: Int = -1, var row: Int = -1)
    val fifoPending: Array[Pending] = Array.fill(subInstNum)(Pending())

    def writeEntry(idx: Int, dataBit: Int, tag: Int): Unit = {
      require(idx < subInstNum)
      table(idx) = ReviseEntry(dataBit, tag)
    }

    def backupChannel(): Unit = {
      for (i <- channels.indices) {
        val src = channels(i)
        channels_bk(i) = channels(i).map(src => DataEntry(src.valid, src.tag, src.data))
      }
    }

    def backupState(): Unit = {
      // --- FIFO deep copy ---
      for (i <- fifos.indices) {
        fifos_bk(i).clear()
        fifos_bk(i) ++= fifos(i)  
      }

      // --- transferState deep copy ---
      for (i <- transferState.indices; j <- transferState(i).indices) {
        transferState_bk(i)(j) = transferState(i)(j)
      }
    }

    def updateChannel(): Unit = {
      for (i <- channels.indices) {
        val src = channels_bk(i)
        channels(i) = channels_bk(i).map(src => DataEntry(src.valid, src.tag, src.data))

      }
    }

    def updateState(): Unit = {
      for (i <- fifos.indices) {
        fifos(i).clear()
        fifos(i) ++= fifos_bk(i)
      }

      for (i <- transferState.indices; j <- transferState(i).indices) {
        transferState(i)(j) = transferState_bk(i)(j)
      }
  
      for (i <- fifo_out_bool.indices) fifo_out_bool(i) = false

    }



    def pushChannel(ch: Int, data: Int, tag: Int): Boolean = {
      channels_bk(ch) match {
        case None =>
          val row = table.indexWhere(_.tag == tag)
          if (row >= 0 && fifos_bk(row).size < fifoDepth) {
            channels_bk(ch) = Some(DataEntry(true, tag, data))
            // if (if_print) println(s"pushed ($tag, $data) into channel $ch")
            true
          } else false
        case Some(reg) =>
          val row = table.indexWhere(_.tag == reg.tag)
          if (row >= 0 && fifos_bk(row).size < fifoDepth && reg.tag != tag && fifo_out_bool(row) && this.choosenChannel==ch) {
            channels_bk(ch) = Some(DataEntry(true, tag, data))
            // if (if_print) println(s"pushed ($tag, $data) into channel $ch")
            true
          } else false
      }
    }

    def canPushChannel(ch: Int, data: Int, tag: Int): Boolean = {
      channels_bk(ch) match {
        case None =>
          val row = table.indexWhere(_.tag == tag)
          row >= 0 && fifos_bk(row).size < fifoDepth

        case Some(reg) =>
          val row = table.indexWhere(_.tag == reg.tag)
          row >= 0 && fifos_bk(row).size < fifoDepth && reg.tag != tag && fifo_out_bool(row) && this.choosenChannel==ch
      }
    }


    def stage(tagsolve: TagSolve): Unit = {
      // --- Channel -> FIFO ---
      val chValids = channels.map(_.exists(_.valid))
      arb9to1.arbitrate(chValids) match {
        case Some(ch) =>
          channels(ch).foreach { reg =>
            val row = table.indexWhere(_.tag == reg.tag)
            if (row >= 0 && fifos(row).size < fifoDepth) {
              fifos_bk(row).enqueue((reg.data, reg.tag))
              channels_bk(ch) = None
            }
            else {
              arb9to1.lowerPriority(ch)
            }
            this.choosenChannel=ch
          }

        case None => // no channel valid
      }

      // --- FIFO -> TagSolve ---
      val fifoValids = fifos.map(_.nonEmpty)
      arb16to1.arbitrate(fifoValids) match {
        case Some(row) =>
          val (data, tag) = fifos(row).front
          val targetVec   = table(row).dataBit

          val instIdxOpt = (0 until subInstNum).find { instIdx =>
            val need = ((targetVec >> instIdx) & 1) == 1
            val sent = transferState(row)(instIdx) == 1
            need && !sent
          }

          instIdxOpt match {
            case None =>
              fifos_bk(row).dequeue()
              java.util.Arrays.fill(transferState(row), 0)

            case Some(instIdx) =>
              val success = tagsolve.pushData(instIdx, din, tag, data)
              
              if (success) {
                fifo_out_bool(row) = true
                if(if_print) println(s"redistribute: ${intToBitArray(targetVec, subInstNum).mkString(",")}")
                if(if_print) println(s"transferState: ${transferState(row).mkString(",")}")
                transferState_bk(row)(instIdx) = 1
                val sentMask = (0 until subInstNum).map(i => transferState_bk(row)(i) << i).sum
                if (sentMask == targetVec) {
                  fifos_bk(row).dequeue()
                  java.util.Arrays.fill(transferState_bk(row), 0)
                }
              } else {
                arb16to1.lowerPriority(row)
              }
          }

        case None => // no FIFO ready
      }
    }





  }

  /**
   * The main CGRA simulator class.
   *
   * @param rows The number of rows in the PE array.
   * @param cols The number of columns in the PE array.
   * @param config The `CgraConfiguration` containing instructions and routing.
   */
  class CGRA(val rows: Int, val cols: Int, config: CgraConfiguration, numInst: Int = 16, numDin: Int = 4, tagWidth: Int = 16, fifoDepth: Int = 8, outnum: Int = 9) {
    private val peArray: Array[Array[PE]] = Array.ofDim[PE](rows, cols)
    private val tagSolveArray: Array[Array[TagSolve]] = Array.ofDim[TagSolve](rows, cols)
    private val inputNewArray: Array[Array[Array[RemuInputNew]]] = Array.ofDim[RemuInputNew](rows, cols, numDin)
    private var globalCycle: Long = 0
    private val nocAdjacencyList: Map[Int, Set[Int]] = Map.empty
    val regs_valid: Array[Int] = config.peRegValid.values.toArray
    val isPEnull: Map[Int, Boolean] = Map((0 until rows * cols).map(i => i -> true): _*)


    // --- Initialize Memory ---
    val memory: Memory = new Memory(size = 65536*16, readPorts = 12, writePorts = 12, accessLatency = 1)

    val cgrainstructions: Map[Int, Array[Instruction]] = config.peInstArray.map { case (peId, instArrays) =>
        val insts = instArrays.zipWithIndex.map { case (instBits, instIdx) =>
          val rawInst = parseBitArrayToLong(instBits)
          val inst    = parseInstructionFromLong(rawInst)
          // if (if_print && rawInst != 0L) {
          //   println(s"PE $peId, Inst $instIdx: ${rawInst.toBinaryString}")
          // }
          inst
        }
        peId -> insts
      }

    // Initialize PEs and build NoC
    for {
      r <- 0 until rows
      c <- 0 until cols
    } {
      val id = r * cols + c

      val instructions: Map[Int, Instruction] = cgrainstructions(id).zipWithIndex.map {
        case (inst, idx) => idx -> inst
      }.to(mutable.Map)

      // instantiate
      val fsms: Map[Int, Map[InstructionType.Value, StatefulInstruction]] =
          (0 until numInst).map(i => i -> createFSMs()).to(mutable.Map)
      peArray(r)(c) = PE(id, r, c, numInst, instructions, fsms)
      val pe = peArray(r)(c)
      tagSolveArray(r)(c) = TagSolve(id, numInst)
      tagSolveArray(r)(c).instructions = instructions
      for (j <- 0 until numDin) {
        inputNewArray(r)(c)(j) = RemuInputNew(j, numInst, fifoDepth, outnum)
      }
      
      // is pe null, skip writing configs to corresponding pe/tagsolve/inputnews
      isPEnull(id) = cgrainstructions(id).forall { inst =>
        inst.opType == InstructionType.NULL
      }

      // write configs into pe, tagsolve, and inputnews 
      if (!isPEnull(id)) {
        println(s"PE $id:")
        println(s"====== Instructions ======")
        for ((idx, inst) <- instructions) {
          if (inst.opType != InstructionType.NULL) println(s"  Inst $idx: $inst")
        }

        for (i <- 0 until numInst) {
          tagSolveArray(r)(c).dataNum(i) = parseBitArrayToInt(config.peDataNum(id)(i))
        }
        if (if_print) {
          println(s"====== DataNum ======")
          for (i <- 0 until numInst) {
            if(pe.instructions(i).opType != InstructionType.NULL) println(s"Inst($i): ${tagSolveArray(r)(c).dataNum(i)}")
          }
        }
        
        for (i <- 0 until numInst) {
          for (j <- 0 until numDin) {
            inputNewArray(r)(c)(j).writeEntry(i, parseBitArrayToInt(config.peRedistForward(id)(i)(j)), parseBitArrayToInt(config.peRedistTag(id)(i)(j)))
          }
        }
        if (if_print) {
          println(s"====== Redistribute ======")
          for (j <- 0 until numDin) {                
            println(s"Din($j):")
            val table = inputNewArray(r)(c)(j).table
            val str = table.filter(_.tag != 0).map { e =>
              s"redistribute: ${intToBitArray(e.dataBit, numInst).mkString(",")}, tag: ${e.tag}"
            }.mkString("\n ")
            println(s"$str")
          }
        }
        if (regs_valid(id) == 1){
          peArray(r)(c).reg_data = 1
          peArray(r)(c).reg_tag = parseBitArrayToInt(config.peRegTag(id))
          peArray(r)(c).reg_valid = config.peRegInputs(id)
          for (j <- 0 until numDin) {
            if (peArray(r)(c).reg_valid(j) == 1) {
              inputNewArray(r)(c)(j).channels(inputNewArray(r)(c)(j).outnum) = Some(DataEntry(true, peArray(r)(c).reg_tag, 1))
              println(s"forced write (true, ${peArray(r)(c).reg_tag}, 1) into channel ${inputNewArray(r)(c)(j).outnum}")
            }
          }
        }
        if (if_print) {
          println(s"====== Reg ======")
          println(s"reg tag: ${peArray(r)(c).reg_tag}")
          println(s"reg valid: ${peArray(r)(c).reg_valid.mkString(", ")}")
        }
        if (if_print) {
          println(s"============================================================")
          println()
        }
        if(outnum==rows*cols)
        nocAdjacencyList(id) = Set(getOrderedDestinations(id, rows, cols): _*)
        else nocAdjacencyList(id) = Set(getOrderedDestinations9(id, rows, cols): _*)
      }
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

    def getInputNew(id: Int): Option[Array[RemuInputNew]] = {
      val r = id / cols
      val c = id % cols
      if (r < rows && c < cols) Some(inputNewArray(r)(c)) else None
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
        val id = r * cols + c
        if (!isPEnull(id)) {
        val pe = peArray(r)(c)
        val tagsolve = tagSolveArray(r)(c)
        val inputnews = inputNewArray(r)(c)

        for (i <- 0 until numDin) {
          inputnews(i).backupChannel()
          inputnews(i).backupState()
        }

        tagsolve.backupInputs()
        
        if (if_print) {
          println(s"=== PE ${pe.id} Channels ===")
          val numChannels = inputnews.head.channels.length
          for (chIdx <- 0 until numChannels) {
            val channelInfos = (0 until numDin).map { i =>
              inputnews(i).channels(chIdx) match {
                case Some(entry) => s"d$i(data=${entry.data},tag=${entry.tag},valid=${entry.valid})"
                case None        => s"d$i(empty)"
              }
            }
            if (!channelInfos.forall(_.contains("empty"))) {
              println(s"  Channel $chIdx: ${channelInfos.mkString(", ")}")
            }
          }

          println(s"=== PE ${pe.id} Fifos ===")
          val numFifos = inputnews.head.fifos.length
          for (fifoIdx <- 0 until numFifos) {
            val fifoInfos = (0 until numDin).map { i =>
              val content = inputnews(i).fifos(fifoIdx).map { case (data, tag) => s"data=$data,tag=$tag" }.mkString(", ")
              s"d$i:[${if (content.isEmpty) "empty" else content}]"
            }
            if (!fifoInfos.forall(_.contains("empty"))) {
              println(s"  FIFO $fifoIdx: ${fifoInfos.mkString(", ")}")
            }
          }
        }


        for (i <- 0 until numDin) {
          inputnews(i).stage(tagsolve)
        }


        for (i <- 0 until numInst) {
          tagsolve.phiLoop(i) = pe.philoopstate.getOrElse(i, 0)
        }

        if (if_print) {
          println(s"=== tagsolve ${pe.id} input regs ===")
          tagsolve.printInputs()
          // println(s"=== tagsolve backup ${pe.id} input regs ===")
          // tagsolve.printbkInputs()
        }

        // val matchresult = tagsolve.step(pe.currentLoadReq,pe.currentStoreReq)

        val matchresult = tagsolve.step()

        matchresult match {
          case Some((instId, outputs, m)) =>
            pe.chosenInst = Some(instId)
            pe.data_in0 = outputs(0)   
            pe.data_in1 = outputs(1)   
            pe.data_in2 = outputs(2)   
            pe.carry_in = outputs(3)
            tagsolve.tagsolve_mask = m
            println(s"PE ${pe.id} choose instruction${instId} ${pe.instructions(instId)}")

            val instruction = pe.instructions.getOrElse(instId, Instruction(InstructionType.NULL, 0, 0, 0, -1))
            if (instruction.immSel == 1) { //d0 is imm
              pe.data_in0 = DataEntry(true, instruction.tag, instruction.immediateValue) 
            } else if (instruction.immSel == 2) { //d1 is imm
              pe.data_in1 = DataEntry(true, instruction.tag, instruction.immediateValue) 
            }
            peExecutionQueue += pe
          case None =>
            if (if_print) println(s"PE ${pe.id} No instruction ready this cycle")
        }
      }
      }

      // Perform execution for all PEs that can run
      peExecutionQueue.foreach { pe =>
        val choseninst = pe.chosenInst.get
        val instruction = pe.instructions.getOrElse(choseninst, Instruction(InstructionType.NULL, 0, 0, 0, -1))
        val tagsolve = tagSolveArray(pe.id / cols)(pe.id % cols)

        var result: Int = 0

        var finalData0 = pe.data_in0.data
        var finalData1 = pe.data_in1.data
        var finalData2 = pe.data_in2.data
        var finalCarry = pe.carry_in.data

        var datain0_valid = pe.data_in0.valid
        var datain1_valid = pe.data_in1.valid
        var datain2_valid = pe.data_in2.valid
        var carryin_valid = pe.carry_in.valid

        // val tagsolve = tagSolveArray(pe.row)(pe.col)

        // Execute the instruction based on the opType
        instruction.opType match {
          case InstructionType.ADD32 => 
            result = finalData0 + finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SUB32 => 
            result = finalData0 - finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.EQ => 
            result = boolToInt(finalData0 == finalData1)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SLT => 
            result = boolToInt(finalData0 < finalData1)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.UGT => 
            result = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) > 0)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.AND => 
            result = finalData0 & finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.OR => 
            result = finalData0 | finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.PRED_OR => 
            if (finalData2 == 1) {
              result = finalData0 | finalData1
              pe.data_out = DataEntry(true, instruction.tag, result)
            } else {
              pe.drop_data(choseninst) = true
            }
          case InstructionType.PRED_OR_REV => 
            if (finalData2 == 0) {
              result = finalData0 | finalData1
              pe.data_out = DataEntry(true, instruction.tag, result)
            } else {
              pe.drop_data(choseninst) = true
            }  
          case InstructionType.SHL => 
            result = finalData0 << finalData1.toInt
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SHR_LOGIC => 
            result = finalData0 >>> finalData1.toInt
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SHR_ARITH => 
            result = finalData0 >> finalData1.toInt
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.XOR => 
            result = finalData0 ^ finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.MUL32 => 
            result = finalData0 * finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.MUL => 
            result = finalData0 * finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          // case InstructionType.UDIV => result = if (finalData1 == 0) 0 else java.lang.Integer.divideUnsigned(finalData0, finalData1)
          // case InstructionType.SDIV => result = if (finalData1 == 0) 0 else finalData0 / finalData1
          // case InstructionType.UREM => result = if (finalData1 == 0) 0 else java.lang.Integer.remainderUnsigned(finalData0, finalData1)
          // case InstructionType.SREM => result = if (finalData1 == 0) 0 else finalData0 % finalData1
          case InstructionType.DATA_TRIGGERED_GEN => 
            result = instruction.immediateValue
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.LOOP_OUT_DATA =>
            val (resulttmp, predResulttmp, loopValid) = pe.fsms(choseninst).get(InstructionType.LOOP_OUT_DATA) match {
              case Some(fsm) =>
                val fsmResult = fsm.step(finalData0, 0, 1-finalData1,
                                        datain0_valid, false, datain1_valid)
                if (fsmResult.drop == true) pe.drop_data(choseninst) = true                
                (
                  fsmResult.dataOut.getOrElse(0),
                  fsmResult.predOut.getOrElse(0),
                  fsmResult.dataOut.isDefined
                )
              case None =>
                (0, 0, false)
            }
            pe.data_out = DataEntry(loopValid, instruction.tag, resulttmp)
          case InstructionType.LOOP_OUT_DATA_REV =>
            val (resulttmp, predResulttmp, loopValid) = pe.fsms(choseninst).get(InstructionType.LOOP_OUT_DATA) match {
              case Some(fsm) =>
                val fsmResult = fsm.step(finalData0, 0, finalData1,
                                        datain0_valid, false, datain1_valid)
                if (fsmResult.drop == true) pe.drop_data(choseninst) = true
                (
                  fsmResult.dataOut.getOrElse(0),
                  fsmResult.predOut.getOrElse(0),
                  fsmResult.dataOut.isDefined
                )
              case None =>
                (0, 0, false)
            }
            pe.data_out = DataEntry(loopValid, instruction.tag, resulttmp)
          case InstructionType.ULT => 
            result = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) < 0)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SGT => 
            result = boolToInt(finalData0 > finalData1)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.LOAD => 
            val book = pe.loadBook.getOrElse(choseninst, LoadBook())
            print(s"STATE: ${book.state} -> ")
            book.state match {
              case LoadState.Idle => 
                val req = memory.load(finalData0)  
                if (req.issue) {     
                  pe.loadBook(choseninst) = LoadBook(state = LoadState.Issue, dataBuf = None)
                  pe.currentLoadReq(choseninst) = Some(req)
                }
                else {
                  pe.loadBook(choseninst) = LoadBook(LoadState.Idle, None)
                  pe.currentLoadReq(choseninst) = None
                }
                tagsolve.lowerPriority(choseninst)
              case LoadState.Issue =>
                pe.currentLoadReq(choseninst) match {
                  case Some(req) if req.valid =>
                    pe.loadBook(choseninst) = LoadBook(state = LoadState.Out, dataBuf = Some(req.data))
                    pe.currentLoadReq(choseninst) = None
                    if (if_print) print(s"  PE(${pe.id}) load from addr ${req.addr} completed, value=${req.data}  ")
                  case _ => 
                }
                tagsolve.lowerPriority(choseninst)
              case LoadState.Out =>
                book.dataBuf match {
                  case Some(d: Int) =>
                    pe.data_out = DataEntry(true, instruction.tag, d)
                  case _ =>
                }
              }
              println(pe.loadBook.getOrElse(choseninst, LoadBook()).state)
          case InstructionType.PRED_LOAD =>
            if (true) {
              val book = pe.loadBook.getOrElse(choseninst, LoadBook())
              print(s"STATE: ${book.state} -> ")
              book.state match {
                case LoadState.Idle => 
                  val req = memory.load(finalData0)  
                  if (req.issue) {     
                    pe.loadBook(choseninst) = LoadBook(state = LoadState.Issue, dataBuf = None)
                    pe.currentLoadReq(choseninst) = Some(req)
                  }
                  else {
                    pe.loadBook(choseninst) = LoadBook(LoadState.Idle, None)
                    pe.currentLoadReq(choseninst) = None
                  }
                  tagsolve.lowerPriority(choseninst)
                case LoadState.Issue =>
                  pe.currentLoadReq(choseninst) match {
                    case Some(req) if req.valid =>
                      pe.loadBook(choseninst) = LoadBook(state = LoadState.Out, dataBuf = Some(req.data))
                      pe.currentLoadReq(choseninst) = None
                      if (if_print) print(pe.currentLoadReq(choseninst))
                      if (if_print) print(s"  PE(${pe.id}) load from addr ${req.addr} completed, value=${req.data}  ")
                    case _ => 
                  }
                  tagsolve.lowerPriority(choseninst)
                case LoadState.Out =>
                  book.dataBuf match {
                    case Some(d: Int) =>
                      pe.data_out = DataEntry(true, instruction.tag, d)
                      if (if_print) print(pe.currentLoadReq(choseninst))
                    case _ =>
                  }
                }
                println(pe.loadBook.getOrElse(choseninst, LoadBook()).state)
              }
          case InstructionType.STORE => 
            val book = pe.storeBook.getOrElse(choseninst, StoreBook())
            print(s"STATE: ${book.state} -> ")
            book.state match {
              case StoreState.Idle =>
                val req = memory.store(finalData1, finalData0)  
                if (req.issue) {
                  pe.storeBook(choseninst) = StoreBook(state = StoreState.Issue)
                  pe.currentStoreReq(choseninst) = Some(req)
                } else {
                  pe.storeBook(choseninst) = StoreBook(StoreState.Idle) 
                  pe.currentStoreReq(choseninst) = None
                  tagsolve.lowerPriority(choseninst)
                }
                
              // case StoreState.Issue =>
              //   pe.currentStoreReq(choseninst) match {
              //     case Some(req) if req.valid =>
              //       pe.storeBook(choseninst) = StoreBook(StoreState.Idle)
              //       pe.currentStoreReq(choseninst) = None
              //     case _ =>
              //   }
              //   tagsolve.lowerPriority(choseninst)
            }
            println(pe.storeBook.getOrElse(choseninst, StoreBook()).state)

          case InstructionType.PRED_STORE_NO_COUT => 
            if (true) {
              val book = pe.storeBook.getOrElse(choseninst, StoreBook())
              print(s"STATE: ${book.state} -> ")
              book.state match {
                case StoreState.Idle =>
                  val req = memory.store(finalData1, finalData0)  
                  if (req.issue) {
                    // pe.storeBook(choseninst) = StoreBook(state = StoreState.Issue)
                    // pe.currentStoreReq(choseninst) = Some(req)
                    pe.storeBook(choseninst) = StoreBook(StoreState.Idle) 
                    pe.currentStoreReq(choseninst) = None
                  } else {
                    pe.storeBook(choseninst) = StoreBook(StoreState.Idle) 
                    pe.currentStoreReq(choseninst) = None
                    tagsolve.lowerPriority(choseninst)
                  }

                // case StoreState.Issue =>
                //   pe.currentStoreReq(choseninst) match {
                //     case Some(req) if req.valid =>
                //       pe.storeBook(choseninst) = StoreBook(StoreState.Idle)
                //       pe.currentStoreReq(choseninst) = None
                //     case _ =>
                //   }
              }
              println(pe.storeBook.getOrElse(choseninst, StoreBook()).state)
            }
          case InstructionType.PRED_STORE_WITH_COUT => 
            if (true) {
              val book = pe.storeBook.getOrElse(choseninst, StoreBook())
              print(s"STATE: ${book.state} -> ")
              book.state match {
                case StoreState.Idle =>
                  val req = memory.store(finalData1, finalData0)  
                  if (req.issue) {
                    pe.storeBook(choseninst) = StoreBook(state = StoreState.Issue)
                    pe.currentStoreReq(choseninst) = Some(req)
                  } else {
                    pe.storeBook(choseninst) = StoreBook(StoreState.Idle) 
                    pe.currentStoreReq(choseninst) = None
                  }
                  tagsolve.lowerPriority(choseninst)
                case StoreState.Issue =>
                  pe.currentStoreReq(choseninst) match {
                    case Some(req) if req.valid =>
                      pe.storeBook(choseninst) = StoreBook(StoreState.Out)
                      pe.currentStoreReq(choseninst) = None
                    case _ =>
                  }
                  tagsolve.lowerPriority(choseninst)
                case StoreState.Out =>
                    pe.data_out = DataEntry(true, instruction.tag, 1)
               }
               println(pe.storeBook.getOrElse(choseninst, StoreBook()).state)
            }
          case InstructionType.STORE_OUT => 
            val book = pe.storeBook.getOrElse(choseninst, StoreBook())
            print(s"STATE: ${book.state} -> ")
            book.state match {
              case StoreState.Idle =>
                val req = memory.store(finalData1, finalData0)  
                if (req.issue) {
                  pe.storeBook(choseninst) = StoreBook(state = StoreState.Issue)
                  pe.currentStoreReq(choseninst) = Some(req)
                } else {
                  pe.storeBook(choseninst) = StoreBook(StoreState.Idle) 
                  pe.currentStoreReq(choseninst) = None
                }
                tagsolve.lowerPriority(choseninst)
              case StoreState.Issue =>
                pe.currentStoreReq(choseninst) match {
                  case Some(req) if req.valid =>
                    pe.storeBook(choseninst) = StoreBook(StoreState.Out)
                    pe.currentStoreReq(choseninst) = None
                  case _ =>
                }
                tagsolve.lowerPriority(choseninst)
              case StoreState.Out =>
                  pe.data_out = DataEntry(true, instruction.tag, 1)
              }
            println(pe.storeBook.getOrElse(choseninst, StoreBook()).state)
          case InstructionType.PHI_BRANCH_DATA => 
            result = if (finalData2 == 1) finalData0 else finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.PHI_LOOP_DATA => 
            val (resulttmp, predResulttmp, loopValid) = pe.fsms(choseninst).get(InstructionType.PHI_LOOP_DATA) match {
              case Some(fsm) =>
                if (if_print) print(finalData0, finalData1, finalData2, datain0_valid, datain1_valid, datain2_valid)
                if (if_print) println(s"current state: ${fsm.state}")
                pe.backup_state(choseninst) = fsm.state
                val fsmResult = fsm.step(finalData0, finalData1, finalData2, datain0_valid, datain1_valid, datain2_valid)
                if (fsmResult.drop == true) pe.drop_data(choseninst) = true
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
            val fsm = pe.fsms(choseninst)(InstructionType.PHI_LOOP_DATA).asInstanceOf[PhiLoopFSM]
            pe.current_state(choseninst) = fsm.state
            if (fsm.state ==  PhiLoopState.Initial) pe.philoopstate(choseninst) = 0 else pe.philoopstate(choseninst) = 1
            pe.data_out = DataEntry(loopValid, instruction.tag, result)
          case InstructionType.NEQ =>  // Unsigned Not Equal
            result = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) != 0)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.ULE =>   // Unsigned Less than or Equal
            result = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) <= 0)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.UGE =>   // Unsigned Greater than or Equal
            result = boolToInt(java.lang.Integer.compareUnsigned(finalData0, finalData1) >= 0)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SLE =>  // Signed Less than or Equal
            result = boolToInt(finalData0 <= finalData1)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SGE =>  // Signed Greater than or Equal
            result = boolToInt(finalData0 >= finalData1)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.NULL => ()

          case InstructionType.FP2INT => 
            result = Math.round(java.lang.Float.intBitsToFloat(finalData0))
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.INT2FP => 
            result = java.lang.Float.floatToIntBits(finalData0.toFloat)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.PRED_FP2INT => 
            if (finalData2 == 1) {
              result = Math.round(java.lang.Float.intBitsToFloat(finalData0))
              pe.data_out = DataEntry(true, instruction.tag, result)
            }
          case InstructionType.PRED_INT2FP => 
            if (finalData2 == 1) {
              result = java.lang.Float.floatToIntBits(finalData0.toFloat)
              pe.data_out = DataEntry(true, instruction.tag, result)
            }
          case InstructionType.UDIV => 
            result = if (finalData1 == 0) 0 else java.lang.Integer.divideUnsigned(finalData0, finalData1)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SDIV => 
            result = if (finalData1 == 0) 0 else finalData0 / finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.UREM => 
            result = if (finalData1 == 0) 0 else java.lang.Integer.remainderUnsigned(finalData0, finalData1)
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.SREM => 
            result = if (finalData1 == 0) 0 else finalData0 % finalData1
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.FMUL => 
            result = java.lang.Float.floatToIntBits(java.lang.Float.intBitsToFloat(finalData0)*java.lang.Float.intBitsToFloat(finalData1))
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.FADD => 
            result = java.lang.Float.floatToIntBits(
              java.lang.Float.intBitsToFloat(finalData0) +
              java.lang.Float.intBitsToFloat(finalData1)
            )
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.FSUB =>
            result = java.lang.Float.floatToIntBits(
              java.lang.Float.intBitsToFloat(finalData0) -
              java.lang.Float.intBitsToFloat(finalData1)
            )
            pe.data_out = DataEntry(true, instruction.tag, result)
          case InstructionType.FDIV =>
            result = java.lang.Float.floatToIntBits(
              java.lang.Float.intBitsToFloat(finalData0) /
              java.lang.Float.intBitsToFloat(finalData1)
            )
            pe.data_out = DataEntry(true, instruction.tag, result)

          // Add other instructions as needed.
          case _ => println(s"  Warning: Unimplemented instruction type: ${instruction.opType}")
        }
        

        // if (instruction.opType == InstructionType.LOOP_OUT_DATA || instruction.opType == InstructionType.LOOP_OUT_DATA_REV 
        // || instruction.opType == InstructionType.PHI_LOOP_DATA) {

        // } else if (instruction.opType == InstructionType.Load || instruction.opType == InstructionType.PRED_LOAD 
        //         || instruction.opType == InstructionType.Store|| instruction.opType == InstructionType.PRED_STORE_NO_COUT ||instruction.opType == InstructionType.PRED_STORE_WITH_COUT) {

        // } else {
        //   pe.data_out = DataEntry(true, instruction.tag, result)
        //   if (pe.outReady == true) {

        //   }
        // }
          

        if (pe.currentLoadReq.get(choseninst).flatten.nonEmpty || pe.currentStoreReq.get(choseninst).flatten.nonEmpty) {
          println(s"  PE(${pe.id}) (${pe.instructions(choseninst)}) ($finalData0,$finalData1,$finalData2,${datain0_valid},${datain1_valid},${datain2_valid}) executed. Result: data=$result. Velid: ${pe.data_out.valid}")
        } else {
          println(s"  PE(${pe.id}) (${pe.instructions(choseninst)}) ($finalData0,$finalData1,$finalData2,${datain0_valid},${datain1_valid},${datain2_valid}) executed. Result: data=$result. Valid: ${pe.data_out.valid}")
        }




      }
      // ROUND 2: Commit results to destination PEs
      println("Round 2: Commit Phase")
      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        val id = r * cols + c
        if (!isPEnull(id)) {
        val pe = peArray(r)(c)
        val tagsolve = tagSolveArray(r)(c)
        var choseninst = -1
        pe.chosenInst match {
          case Some(choseninst1) =>
            choseninst = choseninst1
          case None =>
        }
        
        val peId = pe.id

        if(choseninst != -1)
        if (pe.instructions(choseninst).opType == InstructionType.PRED_STORE_NO_COUT || pe.instructions(choseninst).opType == InstructionType.STORE) {
          tagsolve.clearTagsolveInputs(choseninst)
        }

        if (pe.data_out.valid) {
          val instruction = pe.instructions(choseninst)
          var destinationList = getOrderedDestinations9(peId, rows, cols)
          if(outnum==rows*cols)
            destinationList = getOrderedDestinations(peId, rows, cols)

          // The destination routing is now determined by the outSet arrays
          val outSetD0: Seq[Int] =
            config.peIdToOutSetD0
              .getOrElse(peId, Array.empty[Seq[Int]])
              .lift(choseninst)
              .getOrElse(Seq.empty[Int])

          val outSetD1: Seq[Int] =
            config.peIdToOutSetD1
              .getOrElse(peId, Array.empty[Seq[Int]])
              .lift(choseninst)
              .getOrElse(Seq.empty[Int])

          val outSetD2: Seq[Int] =
            config.peIdToOutSetD2
              .getOrElse(peId, Array.empty[Seq[Int]])
              .lift(choseninst)
              .getOrElse(Seq.empty[Int])

          val outSetC: Seq[Int] =
            config.peIdToOutSetC
              .getOrElse(peId, Array.empty[Seq[Int]])
              .lift(choseninst)
              .getOrElse(Seq.empty[Int])


          // println(s"Type of outSetD0: ${outSetD0.getClass}")
          // println(s"Contents of outSetD0: " + outSetD0.mkString(", "))  
          // println(s"Contents of outSetD1: " + outSetD1.mkString(", "))  
          // println(s"Contents of outSetP0: " + outSetP0.mkString(", "))  
          // println(s"Contents of outSetP1: " + outSetP1.mkString(", "))  

          val dataToRoute = pe.data_out

          var successfullySent = false
          var anyDestinationFound =
            outSetD0.nonEmpty || outSetD1.nonEmpty || outSetD2.nonEmpty || outSetC.nonEmpty

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

          val canSendD0 = if (dataToRoute.valid && outSetD0.nonEmpty) {
            val blocked = outSetD0.filter { destId =>
              var destlist = getOrderedDestinations9(destId, rows, cols)
              if(outnum==rows*cols)
                destlist = getOrderedDestinations(destId, rows, cols)
              val ch = destlist.indexOf(peId)  
              val canPush = getInputNew(destId).map { input =>
                val tinput = input(0)
                tinput.canPushChannel(ch, dataToRoute.data, dataToRoute.tag)
              }.getOrElse(false)
              !canPush
            }
            if (blocked.nonEmpty && if_print) {
              println(s"[DEBUG] PE($peId) blocked at D0 routing: ${blocked.mkString(", ")}")
              if(!dataToRoute.valid) println("No valid")
            }
            blocked.isEmpty
          } else true


          val canSendD1 = if (dataToRoute.valid && outSetD1.nonEmpty) {
            val blocked = outSetD1.filter { destId =>
              var destlist = getOrderedDestinations9(destId, rows, cols)
              if(outnum==rows*cols)
                destlist = getOrderedDestinations(destId, rows, cols)
              val ch = destlist.indexOf(peId)  
              val canPush = getInputNew(destId).map { input =>
                val tinput = input(1)
                tinput.canPushChannel(ch, dataToRoute.data, dataToRoute.tag)
              }.getOrElse(false)
              !canPush
            }
            if (blocked.nonEmpty && if_print) {
              println(s"[DEBUG] PE($peId) blocked at D1 routing: ${blocked.mkString(", ")}")
            }
            blocked.isEmpty
          } else true

          val canSendD2 = if (dataToRoute.valid && outSetD2.nonEmpty) {
            val blocked = outSetD2.filter { destId =>
              var destlist = getOrderedDestinations9(destId, rows, cols)
              if(outnum==rows*cols)
                destlist = getOrderedDestinations(destId, rows, cols)
              val ch = destlist.indexOf(peId)   
              val canPush = getInputNew(destId).map { input =>
                val tinput = input(2)
                tinput.canPushChannel(ch, dataToRoute.data, dataToRoute.tag)
              }.getOrElse(false)
              !canPush
            }
            if (blocked.nonEmpty && if_print) {
              println(s"[DEBUG] PE($peId) blocked at D2 routing: ${blocked.mkString(", ")}")
            }
            blocked.isEmpty
          } else true

          val canSendC = if (dataToRoute.valid && outSetC.nonEmpty) {
            val blocked = outSetC.filter { destId =>
              var destlist = getOrderedDestinations9(destId, rows, cols)
              if(outnum==rows*cols)
                destlist = getOrderedDestinations(destId, rows, cols)
              val ch = destlist.indexOf(peId)   
              val canPush = getInputNew(destId).map { input =>
                val tinput = input(3)
                tinput.canPushChannel(ch, dataToRoute.data, dataToRoute.tag)
              }.getOrElse(false)
              !canPush
            }
            if (blocked.nonEmpty && if_print) {
              println(s"[DEBUG] PE($peId) blocked at C routing: ${blocked.mkString(", ")}")
            }
            blocked.isEmpty
          } else true


          if (anyDestinationFound && canSendD0 && canSendD1 && canSendD2 && canSendC) {
            instruction.opType match {
              case InstructionType.LOAD => 
                val book = pe.loadBook.getOrElse(choseninst, LoadBook())
                book.state match {
                  case LoadState.Out =>
                    book.dataBuf match {
                      case Some(d: Int) =>
                        pe.loadBook(choseninst) = LoadBook(LoadState.Idle, None)
                      case _ =>
                    }
                }
              case InstructionType.PRED_LOAD =>
                val book = pe.loadBook.getOrElse(choseninst, LoadBook())
                book.state match {
                  case LoadState.Out =>
                    book.dataBuf match {
                      case Some(d: Int) =>
                        pe.loadBook(choseninst) = LoadBook(LoadState.Idle, None)
                      case _ =>
                    }
                }
              case InstructionType.PRED_STORE_WITH_COUT => 
                val book = pe.storeBook.getOrElse(choseninst, StoreBook())
                book.state match {
                case StoreState.Out =>
                  pe.storeBook(choseninst) = StoreBook(StoreState.Idle) 
              }
              case InstructionType.STORE_OUT => 
                val book = pe.storeBook.getOrElse(choseninst, StoreBook())
                book.state match {
                case StoreState.Out =>
                  pe.storeBook(choseninst) = StoreBook(StoreState.Idle) 
              }
              case _ => ()
            }
          }


          if (anyDestinationFound && canSendD0 && canSendD1 && canSendD2 && canSendC) {
            if (dataToRoute != null && dataToRoute.valid) {
              val dataEntry = dataToRoute
              // -------- D0 --------
              outSetD0.foreach { destId =>
                var destlist = getOrderedDestinations9(destId, rows, cols)
                if(outnum==rows*cols)
                  destlist = getOrderedDestinations(destId, rows, cols)
                val ch = destlist.indexOf(peId) 
                getInputNew(destId) match {
                  case Some(destPE) if ch >= 0 =>
                    val pushed = destPE(0).pushChannel(ch, dataEntry.data, dataEntry.tag)
                    if (if_print) {
                      if (pushed) println(s"PE($peId) sent data0(${dataEntry.data}) to PE($destId) on ch=$ch")
                      else println(s"PE($peId) failed to send data0(${dataEntry.data}) to PE($destId) on ch=$ch")
                    }
                  case _ =>
                    if (if_print) println(s"Warning: PE($destId) not found, cannot send data0")
                }
              }

              // -------- D1 --------
              outSetD1.foreach { destId =>
                var destlist = getOrderedDestinations9(destId, rows, cols)
                if(outnum==rows*cols) destlist = getOrderedDestinations(destId, rows, cols)
                val ch = destlist.indexOf(peId) 
                getInputNew(destId) match {
                  case Some(destPE) if ch >= 0 =>
                    val pushed = destPE(1).pushChannel(ch, dataEntry.data, dataEntry.tag)
                    if (if_print) {
                      if (pushed) println(s"PE($peId) sent data1(${dataEntry.data}) to PE($destId) on ch=$ch")
                      else println(s"PE($peId) failed to send data1(${dataEntry.data}) to PE($destId) on ch=$ch")
                    }
                  case _ =>
                    if (if_print) println(s"Warning: PE($destId) not found, cannot send data1")
                }
              }

              // -------- D2 --------
              outSetD2.foreach { destId =>
                var destlist = getOrderedDestinations9(destId, rows, cols)
                if(outnum==rows*cols) destlist = getOrderedDestinations(destId, rows, cols)
                val ch = destlist.indexOf(peId) 
                getInputNew(destId) match {
                  case Some(destPE) if ch >= 0 =>
                    val pushed = destPE(2).pushChannel(ch, dataEntry.data, dataEntry.tag)
                    if (if_print) {
                      if (pushed) println(s"PE($peId) sent data2(${dataEntry.data}) to PE($destId) on ch=$ch")
                      else println(s"PE($peId) failed to send data2(${dataEntry.data}) to PE($destId) on ch=$ch")
                    }
                  case _ =>
                    if (if_print) println(s"Warning: PE($destId) not found, cannot send data2")
                }
              }

              // -------- C --------
              outSetC.foreach { destId =>
                var destlist = getOrderedDestinations9(destId, rows, cols)
                if(outnum==rows*cols) destlist = getOrderedDestinations(destId, rows, cols)
                val ch = destlist.indexOf(peId) 
                getInputNew(destId) match {
                  case Some(destPE) if ch >= 0 =>
                    val pushed = destPE(3).pushChannel(ch, dataEntry.data, dataEntry.tag)
                    if (if_print) {
                      if (pushed) println(s"PE($peId) sent control(${dataEntry.data}) to PE($destId) on ch=$ch")
                      else println(s"PE($peId) failed to send control(${dataEntry.data}) to PE($destId) on ch=$ch")
                    }
                  case _ =>
                    if (if_print) println(s"Warning: PE($destId) not found, cannot send control")
                }
              }
            }
            successfullySent = true
            println("Inst ",choseninst," at ", peId," successfully sent!")
          } else {
            successfullySent = false
            // if (dataToRoute.valid) {
            //   if (!canSendD0) {if (if_print) println(s"PE($peId) stalled: data0 FIFO full")}
            //   if (!canSendD1) {if (if_print) println(s"PE($peId) stalled: data1 FIFO full")}
            //   if (!canSendD2) {if (if_print) println(s"PE($peId) stalled: data0 FIFO full")}
            //   if (!canSendC) {if (if_print) println(s"PE($peId) stalled: data1 FIFO full")}
            // }
          }

          if (!successfullySent) {
            tagsolve.lowerPriority(choseninst)

            if (instruction.opType == InstructionType.PHI_LOOP_DATA) {
              pe.fsms(choseninst).get(InstructionType.PHI_LOOP_DATA) match {
                case Some(fsm) =>
                  fsm.state = pe.backup_state(choseninst)
                  if (fsm.state ==  PhiLoopState.Initial) pe.philoopstate(choseninst) = 0 else pe.philoopstate(choseninst) = 1
                case None => 
              }
            }

          } else {
            tagsolve.clearTagsolveInputs(choseninst)
          }

          

          pe.chosenInst = None
          pe.data_in0 = DataEntry(false, -1, 0)
          pe.data_in1 = DataEntry(false, -1, 0)
          pe.data_in2 = DataEntry(false, -1, 0)
          pe.carry_in = DataEntry(false, -1, 0)
          pe.data_out = DataEntry(false, -1, 0)
          pe.carry_out = DataEntry(false, -1, 0)
        } else if (choseninst != -1) {
          val instruction = pe.instructions(choseninst)
          if (instruction.opType == InstructionType.LOOP_OUT_DATA || instruction.opType == InstructionType.LOOP_OUT_DATA_REV || instruction.opType == InstructionType.PHI_LOOP_DATA || instruction.opType == InstructionType.PRED_OR ||  instruction.opType == InstructionType.PRED_OR_REV) {
            if (pe.drop_data(choseninst)) {
              tagsolve.clearTagsolveInputs(choseninst)
            }
          }
        }

      }
      }

      // for {
      //   r <- 0 until rows
      //   c <- 0 until cols
      // } { 
      //   val id = r * cols + c
      //   val inputnews = inputNewArray(r)(c)
      //   if (if_print && !isPEnull(id)) {
      //       println(s"=== PE ${id} Channel_bks ===")
      //       val numChannels = inputnews.head.channels_bk.length
      //       for (chIdx <- 0 until numChannels) {
      //         val channelStr = (0 until numDin).map { i =>
      //           inputnews(i).channels_bk(chIdx) match {
      //             case Some(entry) => s"d$i(data=${entry.data},tag=${entry.tag},valid=${entry.valid})"
      //             case None        => s"d$i(empty)"
      //           }
      //         }.mkString(", ")
      //         println(s"  Channel $chIdx: $channelStr")
      //       }

      //       println(s"=== PE ${id} Fifo_bks ===")
      //       val numFifos = inputnews.head.fifos_bk.length
      //       for (fifoIdx <- 0 until numFifos) {
      //         val fifoStr = (0 until numDin).map { i =>
      //           val content = inputnews(i).fifos_bk(fifoIdx).map { case (data, tag) => s"($data,$tag)" }.mkString(", ")
      //           s"d$i:[$content]"
      //         }.mkString(", ")
      //         println(s"  FIFO $fifoIdx: $fifoStr")
      //       }
      //     }
      // }

      for {
        r <- 0 until rows
        c <- 0 until cols
        j <- 0 until numDin
      } {
        inputNewArray(r)(c)(j).updateChannel()
        inputNewArray(r)(c)(j).updateState()
      }

      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        tagSolveArray(r)(c).updateInputs()
      }

      
      globalCycle += 1
      println("--- End of Cycle ---")
      println("-----------------------------------------------------------------------------------------------------------")
    }

    def isProgramFinished(): Boolean = {
      var i=0
      for {
        r <- 0 until rows
        c <- 0 until cols
      } {
        val pe = peArray(r)(c)
        val tagsolve = tagSolveArray(r)(c)
        if (tagsolve.inD0.exists(_.nonEmpty) ||
            tagsolve.inD1.exists(_.nonEmpty) ||
            tagsolve.inD2.exists(_.nonEmpty) ||
            tagsolve.inCin.exists(_.nonEmpty) ||
            pe.currentStoreReq.exists { case (_, v) => v.nonEmpty } ||
            pe.currentLoadReq.exists { case (_, v) => v.nonEmpty }) {
              
          return false
        }
        for (j <- 0 until numDin) {
          if (inputNewArray(r)(c)(j).fifos.exists(_.nonEmpty) ||
              inputNewArray(r)(c)(j).channels.exists(_.nonEmpty)) {
            return false
          }
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
          var j=0
          while(j<8){
            step()
            j=j+1
          }
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
    var fileName = "arrays_output.txt"
    fileName=args.lift(0).getOrElse("ataxno")
    if(fileName=="ataxno"){
      ATAXTest()
    }
    else if(fileName=="gemmno"){
      gemmTest()
    }
    else if(fileName=="k2mmno"){
      K2MMTest()
    }
    else if(fileName=="k3mmno"){
      K3MMTest()
    }
    else if(fileName=="fftno"){
      FFTTest()
    }
    else if(fileName=="mno"){
      MERGESORTTest()
    }
    else if(fileName=="kmpno"){
      KmpTemporalTest()
    }
    else if(fileName=="nwno"){
      NWTemporalTest()
    }
    else if(fileName=="spmvno"){
      spmvTest()
    }
    // GetTanhTest(fileName, cycles = 8000)
    // MERGESORTTest()
    // spmvTest()
    // TriangularTemporalTest()
    // GetTanhTemporalTest()
    // ATAXTest()
    // gemmTest()
    // K2MMTest()
    // K3MMTest()
    // NWTemporalTest()
    // KmpTemporalTest()
    // FFTTest()
  }

  def GetTanhTemporalTest(fileName: String = "get_tanh_int_t_arrays_output.txt", cycles: Int = 10000): Unit = {
    val cgraRows = 6
    val cgraCols = 6

    try {
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth = 16, numInst = 16)
      var simulator = new simu.CGRA(cgraRows, cgraCols, config)

      var params=2
      var i = 0
      var result =0
      var beta = 0
      var N = 20
      var address = 0
      
      
      
      var A:Array[Int]=new Array[Int](N)
      var addr:Array[Int]=new Array[Int](N)
      var A_bk:Array[Int]=new Array[Int](N)
      var addr_bk:Array[Int]=new Array[Int](N)

      for ( i<-0 to N-1) {
        addr(i) = i
        
        A(i)=Random.nextInt(30)
        
        A_bk(i)=A(i)
        addr_bk(i)=addr(i)
      }

      // Golden result
      for ( i<-0 to N-1){
        address = addr(i);
        beta = A(address);

        if (beta >= 10) {
        result = 1
        } else {
        result = ((beta * beta + 19) * beta * beta + 4) * beta;
        }
        A(address) = result;
      }

      simulator.memory.mem(1) = params+1
      simulator.memory.mem(2) = params+1+N

      for(i<- 0 to N-1){
        simulator.memory.mem(params+1+i) = A_bk(i)
        simulator.memory.mem(params+1+i+N) = addr_bk(i)
      }

      simulator.run(cycles)

      val realResult = Array.tabulate(N) (i => 
        (simulator.memory.mem(params+1+i))
      )

      var pass = true
      for (i <- 0 until N) {
        val diff = math.abs(A(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${A(i)}, got=${realResult(i)}, diff=$diff")
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

  def HistogramTemporalTest(fileName: String = "histogramint.ll_t_arrays_output.txt", cycles: Int = 10000): Unit = {
    val cgraRows = 6
    val cgraCols = 6

    try {
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      println("file parsed")
      val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      var n: Int = 100
      var feature: Array[Int]=new Array[Int](n)
      var weight: Array[Int]=new Array[Int](n)
      var hist: Array[Int]=new Array[Int](n)
      var hist_bk: Array[Int]=new Array[Int](n)

      var i: Int = 0
      var params: Int = 4
      ///////////////////////Init input arrays
      for (i<-0 until n) {
        // feature(i) = Random.nextInt(1000) % 100
        // weight(i) = Random.nextInt(100) % 100
        // hist(i) = Random.nextInt(100) % 100
        // hist_bk(i) = hist(i)
        feature(i) = i
        weight(i) = i
        hist(i) = i
        hist_bk(i) = hist(i)
      }
      ///////////////// local params
      var m: Int = 0
      var wt: Int = 0
      var x: Int = 0
      //////////////////main function
      // println(s"HLS c simulation start") 
      for (i<-0 until n) {
        m = feature(i)
        wt = weight(i)
        x = hist(m)
        hist(m) = x + wt
      }

      simulator.memory.mem(1) = 1+params
      simulator.memory.mem(2) = 1+params+n
      simulator.memory.mem(3) = 1+params+n+n
      simulator.memory.mem(4) = n

      for (i<-0 until n) {
        simulator.memory.mem(params+1+i) = feature(i)
      }
      for (i<-0 until n) {
        simulator.memory.mem(params+1+i+n) = weight(i)
      }
      for (i<-0 until n) {
        simulator.memory.mem(params+1+i+n+n) = hist_bk(i)
      }

      simulator.run(cycles)

      var realResult:Array[Int]=new Array[Int](n)
      for (i<-0 until n) {
        realResult(i) = simulator.memory.mem(params+1+i+n+n)
      }

      var pass = true
      for (i <- 0 until n) {
        val diff = math.abs(hist(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${hist(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }
      assert(pass,"Some values differ")
      if (pass) println("All M values match!")

      println("Simulation finished.")

    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The file '$fileName' was not found.")
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def TriangularTemporalTest(fileName: String = "triangular_t_arrays_output.txt", cycles: Int = 10000): Unit = {
    val cgraRows = 4
    val cgraCols = 4

    try {
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      println("file parsed")
      val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      
      
      ///////////////////////Init input arrays
      
      ///////////////// local params
      
      
      // simulator.run(cycles)


    } catch {
      case e: FileNotFoundException =>
        println(s"Error: The file '$fileName' was not found.")
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        e.printStackTrace()
    }
  }

  def NWTest(fileName: String = "nw.ll_arrays_output.txt", cycles: Int = 200000): Unit = {
    val cgraRows = 23
    val cgraCols = 23

    try {
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols)
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


  def MERGESORTTest(fileName: String = "./benchmark/mno.ll_t_arrays_output.txt", cycles: Int = 50000): Unit = {
    val cgraRows = 6
    val cgraCols = cgraRows

    try {
      // val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      // val fileContent = Source.fromFile(fileName).mkString
      // val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      // val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var msize:Int=64
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


  def ATAXTest(fileName: String = "./benchmark/ataxno.ll_t_arrays_output.txt", cycles: Int = 50000): Unit = {
    val cgraRows = 6
    val cgraCols = 6

    try {
      // val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      // val fileContent = Source.fromFile(fileName).mkString
      // val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      // val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true, if_print_config=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)
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
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols)
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

  def K2MMTest(fileName: String = "./benchmark/k2mmno.ll_t_arrays_output.txt", cycles: Int = 80000): Unit = {
    val cgraRows = 6
    val cgraCols = cgraRows

    try {
      // val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      // val fileContent = Source.fromFile(fileName).mkString
      // val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      // val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var params=2
      var i = 0
      var j = 0
      var l = 0
      var k = 0
      
      var NI = 10
      var NJ = 10
      var NK = 10
      var NL = 10
      
      
      var tmp:Array[Int]=new Array[Int](NI*NJ)
      var A:Array[Int]=new Array[Int](NI*NK)
      var B:Array[Int]=new Array[Int](NK*NJ)
      var C:Array[Int]=new Array[Int](NK*NL)
      var D:Array[Int]=new Array[Int](NI*NL)
      var tmp_bk:Array[Int]=new Array[Int](NI*NJ)
      var D_bk:Array[Int]=new Array[Int](NI*NL)
      

      
      ///////////////////////Init input arrays
      
      // var alpha = Random.nextInt(20)
      // var beta = Random.nextInt(20)
      // for ( i<-0 to NI-1) {
      //     for ( k<-0 to NK-1)
      //         {A(i*NK+k) = Random.nextInt(20)}
      //     for ( l<-0 to NL-1)
      //         {D(i*NL+l) = Random.nextInt(20)
      //         D_bk(i*NL+l) = D(i*NL+l)
      //         }
      // }

      // for (k<-0 to NK-1) {
      //     for ( j <-0 to NJ-1)
      //         {B(k*NJ+j) = Random.nextInt(20)}
      //     for ( l<-0 to NL-1)
      //         {C(k*NL+l) = Random.nextInt(20)}
      // }

      var alpha = 2
      var beta = 2
      for ( i<-0 to NI-1) {
          for ( k<-0 to NK-1)
              {A(i*NK+k) = Random.nextInt(20)}
          for ( l<-0 to NL-1)
              {D(i*NL+l) = Random.nextInt(20)
              D_bk(i*NL+l) = D(i*NL+l)
              }
      }

      for (k<-0 to NK-1) {
          for ( j <-0 to NJ-1)
              {B(k*NJ+j) = Random.nextInt(20)}
          for ( l<-0 to NL-1)
              {C(k*NL+l) = Random.nextInt(20)}
      }


            
          

      ///////////////// local params
      
      

      
      //////////////////main function
      println(s"Remu c simulation start") 
      for (i<-0 to NI-1) {
          for (j <-0 to NJ-1) {
              tmp(i*NJ+j) = 0
              for ( k<-0 to NK-1)
                  {tmp(i*NJ+j) = tmp(i*NJ+j)+ alpha * A(i*NK+k) * B(k*NJ+j)}
          }
      }

      for (i<-0 to NI-1) {
          for (l<-0 to NL-1) {
              D(i*NL+l) = D(i*NL+l)*beta
              for (k<-0 to NJ-1)
                  {D(i*NL+l) = D(i*NL+l)+ tmp(i*NK+k) * C(k*NL+l)}
              // println(D(i*NL+l))
          }
      }
      
      
          

      println(s"Remu c simulation end")
      simulator.memory.mem(1+5*NL*NL) = alpha
      simulator.memory.mem(2+5*NL*NL) = beta

      

      for (i<-0 to NL*NL-1) {
        simulator.memory.mem(1+i) = A(i)
        simulator.memory.mem(1+i+NL*NL) = B(i)
        simulator.memory.mem(1+i+4*NL*NL) = C(i)
        simulator.memory.mem(1+i+2*NL*NL) = tmp_bk(i)
        simulator.memory.mem(1+i+3*NL*NL) = D_bk(i)
      }
      

      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](NL*NL)

      for(i<- 0 to NL*NL-1) {
        realResult(i) = simulator.memory.mem(1+i+3*NL*NL)
      }
      

      var pass = true
      for (i <- 0 until NL*NL) {
        val diff = math.abs(D(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${D(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
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

  def FFTTest(fileName: String = "./benchmark/fftno.ll_t_arrays_output.txt", cycles: Int = 50000): Unit = {
    val cgraRows = 6
    val cgraCols = cgraRows

    try {
      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var params=5
      var msize:Int=64
      var real:Array[Float]=new Array[Float](msize)
      var img:Array[Float]=new Array[Float](msize)
      var real_twid:Array[Float]=new Array[Float](msize)
      var img_twid:Array[Float]=new Array[Float](msize)

      var real_bk:Array[Float]=new Array[Float](msize)
      var img_bk:Array[Float]=new Array[Float](msize)
      ///////////////////////Init input arrays
      for(i<-0 to msize-1){
          real(i)=Random.nextFloat()*256
          real_bk(i)=real(i)
          println("real ",i," is ",real(i))
          img(i)=Random.nextFloat()*256
          img_bk(i)=img(i)
          println("img ",i," is ",img(i))
          real_twid(i)=Random.nextFloat()*256
          img_twid(i)=Random.nextFloat()*256
          println("real_twid ",i," is ",real_twid(i))
          println("img_twid ",i," is ",img_twid(i))
      }

      ///////////////// local params
      var even:Int=0
      var odd:Int=0
      var span:Int=msize/2
      var rootindex:Int=0
      var l:Int=0
      var temp:Float=0.0f
      var log:Int=0
      even=2
      log=32
      println((even << log))
      even=1
      println(even>>1)
      even=0
      log=0
      //////////////////main function
      println(s"HLS c simulation start") 
      while(span>0){
          
          odd=span
          while(odd<msize){
              odd = odd | span
              even = odd ^ span
              temp = real(even) + real(odd)
              real(odd) = real(even) - real(odd)
              real(even) = temp

              temp = img(even) + img(odd)
              img(odd) = img(even) - img(odd)
              img(even) = temp
              println("img odd",img(odd))
              println("img even",img(even))
              println("real odd",real(odd))
              println("real even",real(even))
              rootindex =  (even << log) & (msize - 1);
              println("odd",odd)
              println("even",even)
              println("rootindex",rootindex)
              if (rootindex>0) {
                  temp = real_twid(rootindex) * real(odd) - img_twid(rootindex) * img(odd)
                  img(odd) = real_twid(rootindex) * img(odd) + img_twid(rootindex) * real(odd)
                  real(odd) = temp
                  println("root img odd",img(odd))
                  
                  println("root real odd",real(odd))
              }
              odd=odd+1
              log=log+1
          }
          span=span>>1
          
      }
      println(s"HLS c simulation end")
      simulator.memory.mem(1) = 6
      simulator.memory.mem(2) = 6+msize
      simulator.memory.mem(3) = 6+2*msize
      simulator.memory.mem(4) = 6+3*msize
      simulator.memory.mem(5) = msize

      

      for (i<-0 to msize-1) {
        simulator.memory.mem(6+i) = java.lang.Float.floatToRawIntBits(real_bk(i))
        simulator.memory.mem(6+msize+i) = java.lang.Float.floatToRawIntBits(img_bk(i))
        simulator.memory.mem(6+2*msize+i) = java.lang.Float.floatToRawIntBits(real_twid(i))
        simulator.memory.mem(6+3*msize+i) = java.lang.Float.floatToRawIntBits(img_twid(i))
      }
      

      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](msize)

      for(i<- 0 to msize-1) {
        realResult(i) = simulator.memory.mem(6+i)
      }
      

      var pass = true
      for (i <- 0 until msize) {
        val diff = math.abs(real(i) - java.lang.Float.intBitsToFloat(realResult(i)))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${real(i)}%f, got=${java.lang.Float.intBitsToFloat(realResult(i))}%f, diff=$diff%f")
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

  def K3MMTest(fileName: String = "./benchmark/k3mmno.ll_t_arrays_output.txt", cycles: Int = 50000): Unit = {
    val cgraRows = 6
    val cgraCols = 6

    try {
      // val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      // val fileContent = Source.fromFile(fileName).mkString
      // val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      // val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var params=1
      var i = 0
      var j = 0
      var l = 0
      var k = 0
      var m = 0
      
      var NI = 5
      var NJ = 5
      var NK = 5
      var NL = 5
      var NM = 5
      var N = 5
      
      
      var A:Array[Int]=new Array[Int](NI*NK)
      var B:Array[Int]=new Array[Int](NK*NJ)
      var C:Array[Int]=new Array[Int](NK*NM)
      var D:Array[Int]=new Array[Int](NM*NL)
      var E:Array[Int]=new Array[Int](NI*NL)
      var F:Array[Int]=new Array[Int](NJ*NL)
      var G:Array[Int]=new Array[Int](NI*NL)
      var E_bk:Array[Int]=new Array[Int](NI*NL)
      var F_bk:Array[Int]=new Array[Int](NJ*NL)
      var G_bk:Array[Int]=new Array[Int](NI*NL)
      

      
      ///////////////////////Init input arrays
      
      
      for ( i<-0 to NI-1) {
          for ( k<-0 to NK-1)
              {A(i*NK+k) = Random.nextInt(20)}
          for ( l<-0 to NJ-1)
              {E(i*NJ+l) = Random.nextInt(20)
              E_bk(i*NJ+l) = E(i*NJ+l)
              }
          for ( l<-0 to NL-1)
              {G(i*NL+l) = Random.nextInt(20)
              G_bk(i*NL+l) = G(i*NL+l)
              }    
      }

      for (j<-0 to NJ-1) {
          for ( m <-0 to NM-1)
              {C(j*NM+m) = Random.nextInt(20)}
          for ( l<-0 to NL-1)
              {F(j*NL+l) = Random.nextInt(20)
              F_bk(j*NL+l) = F(j*NL+l)}
      }

      for (k<-0 to NK-1) {
          for ( j <-0 to NJ-1)
              {B(k*NJ+j) = Random.nextInt(20)}
      }

      for ( m <-0 to NM-1) {
          for ( l<-0 to NL-1) {
              D(m*NL+l) = Random.nextInt(20)
          }
      }

      
    

      ///////////////// local params
      
      
      //////////////////main function
      println(s"HLS c simulation start") 

      for ( i<-0 to NI-1) {
          for (j <-0 to NJ-1) {
              E(i*NJ+j) = 0
              for ( k<-0 to NK-1)
                  {E(i*NJ+j) = E(i*NJ+j) + A(i*NK+k) * B(k*NJ+j)}
          }
      }

      for (j <-0 to NJ-1) {
          for ( l<-0 to NL-1) {
              F(j*NL+l) = 0
              for ( m <-0 to NM-1)
                  {F(j*NL+l) = F(j*NL+l) + C(j*NM+m) * D(m*NL+l)}
          }
      }

      for (i<-0 to NI-1) {
          for ( l<-0 to NL-1) {
              G(i*NL+l) = 0
              for (j <-0 to NJ-1)
                  {G(i*NL+l) = G(i*NL+l) + E(i*NL+j) * F(j*NL+l)}
          }
      }



      println(s"HLS c simulation end")

      

      for (i<-0 to NL*NL-1) {
        simulator.memory.mem(1+i) = A(i)
        simulator.memory.mem(1+i+NL*NL) = B(i)
        simulator.memory.mem(1+i+2*NL*NL) = E_bk(i)
        simulator.memory.mem(1+i+3*NL*NL) = C(i)
        
        simulator.memory.mem(1+i+4*NL*NL) = D(i)
        simulator.memory.mem(1+i+5*NL*NL) = F_bk(i)
        simulator.memory.mem(1+i+6*NL*NL) = G_bk(i)
      }
      

      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](NL*NL)

      for(i<- 0 to NL*NL-1) {
        realResult(i) = simulator.memory.mem(1+i+6*NL*NL)
      }
      

      var pass = true
      for (i <- 0 until NL*NL) {
        val diff = math.abs(G(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${G(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
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

  def JacobiTest(fileName: String = "jacobinew.ll_t_arrays_output.txt", cycles: Int = 10000): Unit = {
    val cgraRows = 4
    val cgraCols = 4

    try {
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var params=2
      var i = 0
      var j = 0
      var t = 0
      
      var N = 100
      var TSTEPS = 3
      
      
      
      var A:Array[Int]=new Array[Int](N)
      var B:Array[Int]=new Array[Int](N)
      var A_bk:Array[Int]=new Array[Int](N)
      var B_bk:Array[Int]=new Array[Int](N)
      
      

      
      ///////////////////////Init input arrays
      
      
      for ( i<-0 to N-1) {
          A(i) = Random.nextInt(20)
          A_bk(i)=A(i)
          B(i) = Random.nextInt(20)
          B_bk(i) = B(i)
      }



      ///////////////// local params
      
      
      //////////////////main function
      println(s"HLS c simulation start") 


      for (t <-0 to TSTEPS-1) {
          for (i<-1 to N-2)
            {B(i) = 3 * (A(i-1) + A(i) + A(i+1))}
          for (j<-1 to N-2)
            {A(j) = B(j)}
        }

      
      println(s"HLS c simulation end")
      simulator.memory.mem(1) = 3
      simulator.memory.mem(2) = N+3
      for (i<-0 to N-1) {
        simulator.memory.mem(3+i) = A_bk(i)
        simulator.memory.mem(N+3+i) = B_bk(i)
      }
      

      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](N)

      for(i<- 0 to N-1) {
        realResult(i) = simulator.memory.mem(3+i)
      }
      

      var pass = true
      for (i <- 0 until N) {
        val diff = math.abs(A(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${A(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
          pass = false
        }
      }

      for(i<- 0 to N-1) {
        realResult(i) = simulator.memory.mem(3+N+i)
      }
      

      
      for (i <- 0 until N) {
        val diff = math.abs(B(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${B(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
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


  def NWTemporalTest(fileName: String = "./benchmark/nwno.ll_t_arrays_output.txt", cycles: Int = 300000): Unit = {
    val cgraRows = 6
    val cgraCols = 6

    try {
      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)

      var msize0:Int=32
      var msize1:Int=32

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

  def KmpTemporalTest(fileName: String = "./benchmark/kmpno.ll_t_arrays_output.txt", cycles: Int = 100000): Unit = {
    val cgraRows = 6
    val cgraCols = cgraRows

    try {
      // val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=true, if_print_config=false)
      // val fileContent = Source.fromFile(fileName).mkString
      // val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      // println("file parsed")
      // val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)


      var PatternSize: Int = 4
      var StringSize: Int=128
      var pattern: Array[Int]=new Array[Int](PatternSize)
      var input: Array[Int]=new Array[Int](StringSize)
      var kmpNext: Array[Int]=new Array[Int](PatternSize)
      var kmpNextBK: Array[Int]=new Array[Int](PatternSize)
      var n_matches: Array[Int]=new Array[Int](1)

      var i: Int = 0
      var params: Int = 4
      ///////////////////////Init input arrays
      n_matches(0)=0
      for (i<-0 until PatternSize) {
        // feature(i) = Random.nextInt(1000) % 100
        // weight(i) = Random.nextInt(100) % 100
        // hist(i) = Random.nextInt(100) % 100
        // hist_bk(i) = hist(i)
        pattern(i) = Random.nextInt(1000) % 100
        kmpNext(i) = 0
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

      var pass = true
      for (i <- 0 until 1) {
        val diff = math.abs(n_matches(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${n_matches(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
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

  def gemmTest(fileName: String = "./benchmark/gemmno.ll_t_arrays_output.txt", cycles: Int = 50000): Unit = {
    val cgraRows = 6
    val cgraCols = cgraRows

    try {
      // val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=false, if_print_config=false)
      // val fileContent = Source.fromFile(fileName).mkString
      // val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      // val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true, if_print_config=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)
      

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var params=3
      var i = 0
      var j = 0
      

      var NX = 8
      var NY = 8
      var N = 8
      var t = 0
      var block_size=4

      var m1:Array[Int]=new Array[Int](N*N)
      var m2:Array[Int]=new Array[Int](N*N)
      var prod:Array[Int]=new Array[Int](N*N)
      var prod_bk:Array[Int]=new Array[Int](N*N)

      for (i<-0 to N*N-1) {
        m1(i) = Random.nextInt(512) % 512;
        m2(i) = Random.nextInt(512) % 512;
        prod(i) = 0;
        prod_bk(i)=0;    
      }

      var jj=0
      var kk=0
      var i_row=0;
      var k_row=0
      var temp_x=0
      var mul=0
      while (jj < NY){
        kk=0
        while (kk < NY){
          for ( i <-0 to NY-1){
            for (k <- 0 to block_size-1){
                i_row = i * NY;
                k_row = (k  + kk) * NY;
                temp_x = m1(i_row + k + kk);
                for (j <- 0 to block_size-1){
                    mul = temp_x * m2(k_row + j + jj)
                    prod(i_row + j + jj) = prod(i_row + j + jj)+mul;
                }
            }
          }
          kk = kk+ block_size
        }
        jj =jj+ block_size
      }

      simulator.memory.mem(1) = 1+params
      simulator.memory.mem(2) = 1+params+N*N
      simulator.memory.mem(3) = 1+params+N*N+N*N

      for (i<-0 to N*N-1) {
        simulator.memory.mem(params+1+i) = m1(i)
        simulator.memory.mem(params+1+i+N*N) = m2(i)
        simulator.memory.mem(params+1+i+N*N+N*N) = prod_bk(i)
      }
      

      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](N*N)

      for(i<- 0 to N*N-1) {
        realResult(i) = simulator.memory.mem(1+params+N*N+N*N+i)
      }
      

      var pass = true
      for (i <- 0 until N*N) {
        val diff = math.abs(prod(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${prod(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
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



  def spmvTest(fileName: String = "./benchmark/spmvno.ll_t_arrays_output.txt", cycles: Int = 50000): Unit = {
    val cgraRows = 6
    val cgraCols = 6

    try {
      // val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, if_print=false, if_print_config=false)
      // val fileContent = Source.fromFile(fileName).mkString
      // val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols)
      // val simulator = new simu.CGRA(cgraRows, cgraCols, config)

      val DATA_WIDTH: Int = 32
      val tagWidth: Int = 16
      val numInst: Int = 16
      val outnum: Int = 9
      val fifoDepth: Int = 8
      val numDin: Int = 4
      val simu = new CgraSimulatorTemporal(cgraRows, cgraCols, DATA_WIDTH, tagWidth, numInst, outnum, fifoDepth, numDin, if_print=true, if_print_config=true)
      val fileContent = Source.fromFile(fileName).mkString
      val config = simu.readConfigFromFile(fileContent, cgraRows, cgraCols, tagWidth, numInst, outnum)
      val simulator = new simu.CGRA(cgraRows, cgraCols, config, numInst, numDin, tagWidth, fifoDepth, outnum)
      

      // val A: Array[Float] = Array.tabulate(100)(i => java.lang.Float.intBitsToFloat(simulator.memory.mem(i + 3)))
      // val addr: Array[Int] = Array.tabulate(100)(i => simulator.memory.mem(i + 103))
      var params=5
      var i = 0
      var j = 0
      

      var NX = 8
      var NY = 8
      var N = 50
      var t = 0
      var block_size=4

      var rowDelimiters:Array[Int]=new Array[Int](N+1)
      var vals:Array[Int]=new Array[Int](N)
      var vec:Array[Int]=new Array[Int](N)
      var cols:Array[Int]=new Array[Int](N)
      var out:Array[Int]=new Array[Int](N)

      for (i<-0 to N-1) {
        if(i%2==0){
          rowDelimiters(i) = Random.nextInt(N/2) % (N/2);
          if(rowDelimiters(i)<0){
            rowDelimiters(i)=N+rowDelimiters(i)
          }
        }
        else{
          rowDelimiters(i) = (Random.nextInt(N/2) % (N/2))+N/2;
          if(rowDelimiters(i)<0){
            rowDelimiters(i)=N+rowDelimiters(i)
          }
        }
        
        vals(i) = Random.nextInt(N) % N;
        vec(i) = Random.nextInt(N) % N;
        cols(i)=Random.nextInt(N) % N;   
        if(cols(i)<0){
          cols(i)=N+cols(i)
        }
      }
      rowDelimiters(N)=N
      var jj=0
      var kk=0
      var i_row=0;
      var k_row=0
      var temp_x=0
      var mul=0
      
      var sum=0
      var Si=0

      for(i <- 0 to N-1){
          sum = 0; 
          Si = 0;
          var tmp_begin = rowDelimiters(i);
          var tmp_end = rowDelimiters(i+1);
          if(tmp_begin>=tmp_end){

          }
          else{
            for (j <- tmp_begin to tmp_end-1){
                Si = vals(j) * vec(cols(j));
                sum = sum + Si;
            }
          }
          out(i) = sum;
      }

      simulator.memory.mem(1) = 1+params
      simulator.memory.mem(2) = 1+params+N
      simulator.memory.mem(3) = 1+params+N*2
      simulator.memory.mem(4) = 1+params+N*2+N+1
      simulator.memory.mem(5) = 1+params+N*2+N+1+N

      for (i<-0 to N-1) {
        simulator.memory.mem(params+1+i) = vals(i)
        simulator.memory.mem(params+1+i+N) = cols(i)
        simulator.memory.mem(params+1+i+N*2) = rowDelimiters(i)
        simulator.memory.mem(params+1+i+N*2+N+1) = vec(i)
      }
      simulator.memory.mem(params+1+N+N*2) = rowDelimiters(N)

      simulator.run(cycles)


      var realResult:Array[Int]=new Array[Int](N)

      for(i<- 0 to N-1) {
        realResult(i) = simulator.memory.mem(1+params+N*2+N+1+N+i)
      }
      

      var pass = true
      for (i <- 0 until N) {
        val diff = math.abs(out(i) - realResult(i))
        if (diff > 1e-5) {
          println(f"Mismatch at index $i: expected=${out(i)}%f, got=${realResult(i)}%f, diff=$diff%f")
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
}


