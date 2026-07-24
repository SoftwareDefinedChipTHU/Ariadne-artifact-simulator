

// import SyncMEM._
import chisel3._
import chisel3.util._
import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex
import scala.collection.mutable._
import scala.util.control._
import scala.util.Random 
import java.io.PrintWriter
import java.io.File
import scala.language.postfixOps



// import SyncMEM._
// import chisel3._
// import chisel3.util._
// import scala.io.{BufferedSource, Source}
// import scala.util.matching.Regex
// import scala.collection.mutable._
// import scala.util.control._
// import scala.util.Random 
// import scala.language.postfixOps

package BackendCodeEmitter{
//////////////read the dot graph, and output the circuit
class utilsHLS{
def getPENum(srcArr: Array[String], total:Int,toFind:String):Int ={
    var num:Int=0
    val loop = new Breaks;
    loop.breakable{
        for(i<-0 to total-1){
            num=num+1
            if(srcArr(i).compareTo(toFind)==0){
                loop.break
            }
            
        }
    }
    
    return num-1
}

def isPred(opCode:String,opNum:String):Int={
    if(opCode.compareTo("loadi")==0){
        if(opNum.compareTo("1")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("storei")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("add")==0||opCode.compareTo("sub")==0||opCode.compareTo("eq")==0||opCode.compareTo("lt")==0||opCode.compareTo("gt")==0||opCode.compareTo("slt")==0||opCode.compareTo("sgt")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("band")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bor")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bxor")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bshl")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bshr")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bshrl")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("mul")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("div")==0||opCode.compareTo("rem")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("udiv")==0||opCode.compareTo("urem")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("fadd")==0||opCode.compareTo("fsub")==0||opCode.compareTo("fmul")==0||opCode.compareTo("fdiv")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("uitofp")==0||opCode.compareTo("sitofp")==0||opCode.compareTo("fptoui")==0||opCode.compareTo("fptosi")==0){
        if(opNum.compareTo("1")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("gen")==0){
        if(opNum.compareTo("1")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("phi")==0||opCode.compareTo("phiLoop")==0){
        if(opNum.compareTo("2")==0){
            return 1
        }
        else if(opNum.compareTo("3")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("controlphi")==0||opCode.compareTo("controlphiLoop")==0){
        if(opNum.compareTo("1")==0){
            return 1
        }
        else if(opNum.compareTo("2")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("loopout")==0){
        if(opNum.compareTo("1")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("mergepred")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else{
            return 2
        }
    }
    else{
        return 0
    }
}

def isData(opCode:String,opNum:String):Int={
    if(opCode.compareTo("loadi")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("storei")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("add")==0||opCode.compareTo("sub")==0||opCode.compareTo("eq")==0||opCode.compareTo("lt")==0||opCode.compareTo("gt")==0||opCode.compareTo("slt")==0||opCode.compareTo("sgt")==0||opCode.compareTo("uneq")==0||opCode.compareTo("leq")==0||opCode.compareTo("geq")==0||opCode.compareTo("sleq")==0||opCode.compareTo("sgeq")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("band")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bor")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bxor")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bshl")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bshr")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("bshrl")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("mul")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("div")==0||opCode.compareTo("rem")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("udiv")==0||opCode.compareTo("urem")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("fadd")==0||opCode.compareTo("fsub")==0||opCode.compareTo("fmul")==0||opCode.compareTo("fdiv")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("uitofp")==0||opCode.compareTo("sitofp")==0||opCode.compareTo("fptoui")==0||opCode.compareTo("fptosi")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("gen")==0){
        
        if(opNum.compareTo("0")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("phi")==0||opCode.compareTo("phiLoop")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else if(opNum.compareTo("1")==0){
            return 2
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("controlphi")==0||opCode.compareTo("controlphiLoop")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else if(opCode.compareTo("loopout")==0){
        if(opNum.compareTo("0")==0){
            return 1
        }
        else{
            return 0
        }
    }
    else{
        return 0
    }
}
}

class CodeEmit(fname: String, dataWidth1: Int,mapfile1:String,Width1:Int,CGRASize1:Int,perInst1:Int,maxEdges1:Int,tagWidth1:Int=16,outSize1:Int=13) {
    val filename = fname
    val dataWidth = dataWidth1
    val mapfile = mapfile1
    val Width = Width1
    val CGRASize=CGRASize1
    val perInst = perInst1
    val maxInst = CGRASize1*perInst1
    val maxEdges = maxEdges1
    var outSize=outSize1
    val tagWidth=tagWidth1

    var CGRA_array: Array[Array[Int]] = Array.ofDim[Int](CGRASize1, CGRASize1)


    var originalInstArray: Array[Array[Int]] = Array.ofDim[Int](maxInst, dataWidth*2)
    var fanoutInstArray: Array[Array[Int]] = Array.ofDim[Int](maxInst, dataWidth*2)
    var finalInstArray: Array[Array[Int]] = Array.ofDim[Int](maxInst, dataWidth*2)
    var totalInstNum: Int = 0
    var appSize=0
    var tagConnections: Array[Array[Int]] = new Array[Array[Int]](maxEdges)
    var placements:Array[Int]=new Array[Int](maxInst)
    var finalplacements:Array[Int]=new Array[Int](maxInst)
    var originalMappingTags:Array[Int]=new Array[Int](maxInst)
    var fanoutMappingTags:Array[Int]=new Array[Int](maxInst)
    var connectPaths: Array[Array[Int]] = new Array[Array[Int]](maxEdges)
    var connectPathsLengths: Array[Int]=new Array[Int](maxEdges)

    var connectPredNum:Array[Int]=new Array[Int](maxInst)
    var connectDataNum:Array[Int]=new Array[Int](maxInst)

    var OutDataPE: Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var OutDataPENum: Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var OutPredPE: Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var OutPredPENum: Array[Array[Int]] = new Array[Array[Int]](maxInst)


    var finalConnection: Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var oriConnection: Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var finalConnectionbk: Array[Array[Int]] = new Array[Array[Int]](maxInst)

    var outSetD0:Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var outSetD1:Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var outSetP0:Array[Array[Int]] = new Array[Array[Int]](maxInst)
    var outSetP1:Array[Array[Int]] = new Array[Array[Int]](maxInst)

    var finalInstForRemu:Array[Array[Int]] = Array.ofDim[Int](maxInst, dataWidth*2)
    var remuoutSetD0:Array[Array[Int]] = Array.ofDim[Int](maxInst, outSize)
    var remuoutSetD1:Array[Array[Int]] = Array.ofDim[Int](maxInst, outSize)
    var remuoutSetD2:Array[Array[Int]] = Array.ofDim[Int](maxInst, outSize)
    var remudataNum:Array[Array[Int]] = Array.ofDim[Int](maxInst, 4)
    var remuTagresolve0:Array[Array[Int]] = Array.ofDim[Int](maxInst, 3)
    var remuTagresolve1:Array[Array[Int]] = Array.ofDim[Int](maxInst, 3)
    var remuTagresolveNum:Array[Int]=new Array[Int](maxInst)

    var remuFromReg:Array[Int]=new Array[Int](maxInst)
    var regconfig:Array[Array[Int]] = Array.ofDim[Int](maxInst, dataWidth)
    var regconfigTag:Array[Int] = new Array[Int](maxInst)
    var whichReg:Array[Int]=new Array[Int](maxInst)

    // var remuRedistributor:Array[Array[Int]] = Array.ofDim[Int](maxInst, 4*perInst+4)
    // var remuRegInst:Array[Array[Int]] = Array.ofDim[Int](maxInst, 2*tagWidth)
    // var remuRegV:Array[Array[Int]] = Array.ofDim[Int](maxInst, 4+tagWidth)
    // var remuRegIn:Array[Array[Int]] = Array.ofDim[Int](maxInst, dataWidth)

    var remuRegTags:Array[Array[Int]] = Array.ofDim[Int](CGRASize,tagWidth)
    var remuRegValid:Array[Int]=new Array[Int](CGRASize)
    var remuRegV:Array[Array[Int]] = Array.ofDim[Int](CGRASize, 4+tagWidth)

    var remuPEInst:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, perInst,dataWidth*2)
    var remuPETag:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, perInst,tagWidth)
    var remuPERedist:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, 4*perInst,perInst+tagWidth)
    // var remuPERedistor:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, perInst,4*perInst+tagWidth)
    var remuPEOut:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, perInst,outSize*3)
    var remuPEData:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, perInst,4)

    var InstRemuNo:Array[Int]=new Array[Int](maxInst)

    def debugPrintInst():Int={
        var k=0
        var b=0
        var j=0
        remuTagresolve0.zipWithIndex.take(this.totalInstNum).foreach { case (value, i) =>
            k=value(0)
            b=value(1)
            j=value(2)
        println(s"remuTagresolve0($i) high 32 bits as decimal = $k,$b,$j")
        }
        remuTagresolve1.zipWithIndex.take(this.totalInstNum).foreach { case (value, i) =>
            k=value(0)
            b=value(1)
            j=value(2)
        println(s"remuTagresolve1($i) high 32 bits as decimal = $k,$b,$j")
        }
        return 0
    }

    def CGRAGen(routerLike:Int=1,fully:Int=0):Int = {
        val width=this.Width
        val arraySize=width*width
        // var CGRA_array = Array.ofDim[Int](arraySize, arraySize)
        var k=0
        // val routerLike=1
        
        for (i <- 0 to arraySize-1){
            
            for (j <- 0 to arraySize-1){
                this.CGRA_array(i)(j) = 0
            }
            this.CGRA_array(i)(i) = 1
            this.CGRA_array(i)((i % width + 1) % width + (i / width) * width) = 1
            this.CGRA_array(i)((i % width - 1 + width) % width + (i / width) * width) = 1
            this.CGRA_array(i)((i % width) % width + (((i / width) + 1) % width) * width) = 1
            this.CGRA_array(i)((i % width) % width + (((i / width) - 1 + width) % width) * width) = 1
            
            if(routerLike==0){
                this.CGRA_array(i)(((i % width + 2)%width + ((i / width)) * width)) = 1
                
                this.CGRA_array(i)(((i % width - 2+width)%width + ((i / width)) * width)) = 1
                this.CGRA_array(i)((i % width + (((i / width) + 2)%width) * width)) = 1
                this.CGRA_array(i)((i % width + (((i / width) - 2+width)%width) * width)) = 1
            }
            this.CGRA_array(i)(((i % width + 1)%width + (((i / width) + 1)%width) * width)) = 1
            this.CGRA_array(i)(((i % width + 1)%width + (((i / width) - 1+width)%width) * width)) = 1
            this.CGRA_array(i)(((i % width - 1+width)%width + (((i / width) + 1)%width) * width)) = 1
            this.CGRA_array(i)(((i % width - 1+width)%width + (((i / width) - 1+width)%width) * width)) = 1
            
            
        }
        for (i <- 0 to arraySize-1){
            
            for (j <- 0 to arraySize-1){
                if(fully == 1)
                    this.CGRA_array(i)(j) = 1
            }

        }
        var outNum=0
        for(i<- 0 to arraySize-1){
            if(this.CGRA_array(0)(i)==1){
                outNum=outNum+1
            }
        }
        this.outSize = outNum
        println("PE size is:")
        println(arraySize)
        println("Per PE connection is:")
        println(outNum)

        return 0
    }

    def parseMapping(mapfile:String):Int = {
        val filePath = mapfile
        // val filePath = "E:\\mapfirst\\get_tanh.dot.map"
        val placementPattern: Regex = "(?s)placement results!\\s*\\[(.*?)\\]\\s*(?=original ops!)".r
        val originalOpsPattern: Regex = "original ops! \\[(.*?)\\]".r
        val fanoutOpsPattern: Regex = "fanout reduced ops! \\[(.*?)\\]".r
        val connectPathsPattern: Regex = "connection paths! \\[(.*)\\]".r
        val connectPattern: Regex = "connections! \\[(.*)\\]\\s*(?=connection paths!)".r
        
        val source = Source.fromFile(filePath)
        val fileContent = source.getLines().mkString(" ")
        source.close()
        // println(fileContent)
        val cleanedContent = fileContent.replaceAll("\\s+", " ")
        
        val placementMatch = placementPattern.findFirstMatchIn(cleanedContent)
        // placementMatch match {
        // case Some(matched) =>
        //     val extracted = matched.group(1)
        //     println(s"Extracted placement data: $extracted")
            
        //     val items = extracted.split("\\]\\s*\\[")
        //     this.placements = items.map(_.replaceAll("[\\[\\]]", "").trim)
        //                         .map(_.split("\\s+").filter(_.nonEmpty).map(_.toInt))
        //                         .map(arr => if (arr.length > 1) arr(1) else 0)
            
        // case None =>
        //     println("No placement results found!")
        //     this.placements = Array.empty[Int]
        // }

        this.placements = placementMatch match {
            case Some(matched) =>
                matched.group(1)
                    .split("\\]\\s*\\[")
                    .map(_.replaceAll("[\\[\\]]", "").trim)
                    .map(_.split("\\s+").filter(_.nonEmpty).map(_.toInt))
                    .map(_(1))
            case None =>
                println("No placement results found!")  
                Array.empty[Int]
        }
        
        
        
        this.originalMappingTags = originalOpsPattern.findFirstMatchIn(fileContent) match {
        case Some(matched) =>
            matched.group(1).split(" ").filter(_.nonEmpty).map(_.toInt)
        case None =>
            println("No original ops found!")
            Array.empty[Int]
        }
        
        var fanoutMappingTagsCount: Int = 0
        this.fanoutMappingTags = fanoutOpsPattern.findFirstMatchIn(fileContent) match {
        case Some(matched) =>
            matched.group(1).split(" ").filter(_.nonEmpty).map(_.toInt)
            // fanoutMappingTagsCount = fanoutMappingTagsmatch.length
        case None =>
            println("No fanout reduced ops found!")
            Array.empty[Int]
        }

        this.connectPaths = connectPathsPattern.findFirstMatchIn(fileContent) match {
        case Some(matched) =>
            val extractedString = matched.group(1)
            println(s"Extracted connection paths string: $extractedString")

            extractedString
            .split("\\], \\[")
            .map(_.replaceAll("[\\[\\]]", ""))
            .map(_.split(", ").map(_.toInt))

        case None =>
            println("No connection paths found!")
            Array.empty[Array[Int]]
        }

        this.tagConnections = connectPattern.findFirstMatchIn(fileContent) match {
        case Some(matched) =>
            val extractedString = matched.group(1)
            // println(s"Extracted connection paths string: $extractedString")

            extractedString
            .split("\\], \\[")
            .map(_.replaceAll("[\\[\\]]", ""))
            .map(_.split(", ").map(_.toInt))

        case None =>
            println("No connection paths found!")
            Array.empty[Array[Int]]
        }

        this.connectPathsLengths = this.connectPaths.map(_.length)
        println("connectPathsLengths:")
        println(connectPathsLengths.mkString(", "))

        // placements.zipWithIndex.foreach { case (value, index) =>
        //     println(s"placements($index) = $value")
        // }
        
        originalMappingTags.zipWithIndex
        .foreach { case (value, index) =>
            println(s"originalMappingTags($index) = $value")
        }
        
        // fanoutMappingTags.zipWithIndex
        // .foreach { case (value, index) =>
        //     println(s"fanoutMappingTags($index) = $value")
        // }

        // tagConnections.zipWithIndex.foreach { case (value, index) =>
        // println(s"tagConnections($index) = [${value.mkString(", ")}]")
        // // var sst=value(0)
        // // println(s"$sst")
        // }

        // connectPaths.zipWithIndex.foreach { case (value, index) =>
        // println(s"connectPaths($index) = [${value.mkString(", ")}]")
        // }



        var lastValidIndex = -1
        for (i <- fanoutMappingTags.indices) {
            val tag = fanoutMappingTags(i)
            val index = originalMappingTags.indexOf(tag)
            
            if (index != -1) {
                this.fanoutInstArray(i) = this.originalInstArray(index).clone()
                lastValidIndex = index
            } else if (lastValidIndex != -1) {//add routing nodes
                this.fanoutInstArray(i) = this.originalInstArray(lastValidIndex).clone()
            }
        }
        //go through the tagconnections
        this.tagConnections.zipWithIndex.foreach { case (value, index) =>
            {
                // println(s"tagConnections($index) = [${value.mkString(", ")}]")
                // val (src, dst) = value
                var src0=value(0)
                val dst0=value(1)
                var last=this.connectPaths(index)(this.connectPathsLengths(index)-1)
                assert(this.placements(dst0)==last)
            }
        
        }
        
        
        // fanoutInstArray.zipWithIndex.take(fanoutMappingTags.length).foreach { case (value, index) =>
        //     println(s"fanoutInstArray($index) = ${value.mkString(",")}")
        // }
        return 0
    }

    def parseDot(fname: String, dataWidth: Int):Int = {

        assert(dataWidth<=32)
    
        val pt=new utilsHLS
        var sourse=Source.fromFile(fname)
        var lineIterator=sourse.getLines()
        val pattern = "([a-zA-Z]+\\d+).opcode=([a-zA-Z]+)..level=\\d+.*".r
        val pattern2="([a-zA-Z]+\\d+)\\s*->\\s*([a-zA-Z]+\\d+).operand=(\\d+).*".r
        val pattern3="([a-zA-Z]+\\d+)\\s*->\\s*([a-zA-Z]+\\d+).operand=(\\d+)..size=\\d+..preverse=(\\d+).*".r
        val constPattern="([a-zA-Z]+\\d+).opcode=const..data=(.?\\d+)..level=\\d+.*;".r
        val constPatternF="([a-zA-Z]+\\d+).opcode=const..data=(.?\\d+\\.*\\d*)..level=\\d+.*;".r
        val cp="(const\\d+).*".r
        

        var opNum: Int=0
        for(line<-lineIterator){
            line match {
                case pattern(opId, opCode) =>{
                    // println(opId)
                    opNum=opNum+1
                }
                case _ =>{}
            }
        }
        sourse.close()
        println("total nodes ",(opNum).toString())
        
        
        
        val PEarrName: Array[String] = new Array[String](opNum)
        val PEarrType: Array[String] = new Array[String](opNum)
        val PEarrOutData: Array[Int] = new Array[Int](opNum)
        val PEarrOutPred: Array[Int] = new Array[Int](opNum)
        val PEarrInPred: Array[Int] = new Array[Int](opNum)
        val PEarrInData: Array[Int] = new Array[Int](opNum)
        // val PEarrImm=Wire(Vec(opNum,UInt(dataWidth.W)))
        val PEarrImm: Array[Int]=new Array[Int](opNum)
        val PEarrImmHas:Array[Int] = new Array[Int](opNum)
        val PEarrLSHas:Array[Int] = new Array[Int](opNum)

        val PEarrPredR:Array[Int] = new Array[Int](opNum)

        val PEarrOutDataPE: Array[Array[Int]] = new Array[Array[Int]](opNum)
        val PEarrOutDataPENum: Array[Array[Int]] = new Array[Array[Int]](opNum)
        val PEarrOutPredPE: Array[Array[Int]] = new Array[Array[Int]](opNum)
        val PEarrOutPredPENum: Array[Array[Int]] = new Array[Array[Int]](opNum)
        // val PEInstArr=Wire(Vec(opNum,Vec(dataWidth*2,UInt(1.W))))
        val PEInstArr: Array[Array[Int]] = new Array[Array[Int]](opNum)
        for(i<- 0 to opNum-1){
            PEInstArr(i)=new Array[Int](dataWidth*2)
            for(j<-0 to dataWidth*2-1){
                PEInstArr(i)(j)=0
            }
            PEarrOutPredPENum(i)=new Array[Int](opNum)
            PEarrOutDataPENum(i)=new Array[Int](opNum)
            PEarrOutPredPE(i)=new Array[Int](opNum)
            PEarrOutDataPE(i)=new Array[Int](opNum)
            for(j<-0 to opNum-1){
                PEarrOutPredPE(i)(j)=0
                PEarrOutDataPE(i)(j)=0
                PEarrOutPredPENum(i)(j)=0
                PEarrOutDataPENum(i)(j)=0
            }
            PEarrImmHas(i)=0
            PEarrLSHas(i)=0
            PEarrPredR(i)=0
            PEarrImm(i)=0
        }
        
        for(i<- 0 to this.maxInst-1){
           
            this.OutPredPENum(i)=new Array[Int](this.maxInst)
            this.OutDataPENum(i)=new Array[Int](this.maxInst)
            this.OutPredPE(i)=new Array[Int](this.maxInst)
            this.OutDataPE(i)=new Array[Int](this.maxInst)
            for(j<-0 to this.maxInst-1){
                this.OutPredPE(i)(j)=0
                this.OutDataPE(i)(j)=0
                this.OutPredPENum(i)(j)=0
                this.OutDataPENum(i)(j)=0
            }
            
        }
        opNum=0
        var LSNum:Int=0
        //parse the line and choose pes
        sourse=Source.fromFile(fname)
        lineIterator=sourse.getLines()
        for(line<-lineIterator){
            // println(line)
            line match {
                case pattern(opId, opCode) =>{
                    //create new PE
                    println("PE ",opNum," is ",opCode," with name ",opId)
                    if(opCode.compareTo("loadi")==0||opCode.compareTo("storei")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        PEarrLSHas(opNum)=1
                        
                        LSNum=LSNum+1
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("add")==0||opCode.compareTo("sub")==0||opCode.compareTo("eq")==0||opCode.compareTo("lt")==0||opCode.compareTo("gt")==0||opCode.compareTo("slt")==0||opCode.compareTo("sgt")==0||opCode.compareTo("uneq")==0||opCode.compareTo("leq")==0||opCode.compareTo("geq")==0||opCode.compareTo("sleq")==0||opCode.compareTo("sgeq")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("band")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("bor")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("bxor")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("bshl")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("bshr")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("bshrl")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("mul")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("div")==0||opCode.compareTo("rem")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("udiv")==0||opCode.compareTo("urem")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("fptoui")==0||opCode.compareTo("fptosi")==0||opCode.compareTo("uitofp")==0||opCode.compareTo("sitofp")==0||opCode.compareTo("fadd")==0||opCode.compareTo("fsub")==0||opCode.compareTo("fmul")==0||opCode.compareTo("fdiv")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("gen")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("phi")==0||opCode.compareTo("controlphi")==0||opCode.compareTo("controlphiLoop")==0||opCode.compareTo("phiLoop")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("loopout")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else if(opCode.compareTo("mergepred")==0){
                        
                        PEarrName(opNum)=opId
                        PEarrType(opNum)=opCode
                        PEarrOutData(opNum)=0
                        PEarrOutPred(opNum)=0
                        PEarrInPred(opNum)=0
                        PEarrInData(opNum)=0
                        
                        opNum=opNum+1
                    }
                    else{
                        
                    }
                    // println("Added name is ",PEarrName(opNum-1))
                }
                // case pattern2(nameEdge, opIdEdge, operands) =>{
                    
                // }
                case _ =>{}
                // case _ => println(line)
            }
            
        }
        sourse.close()
        var Numconst:Int=0
        println("total memory ops ", LSNum.toString())
        ///parse constant
        sourse=Source.fromFile(fname)
        lineIterator=sourse.getLines()
        for(line<-lineIterator){
            line match {
                case constPatternF(constName, constNum) =>{
                    Numconst=Numconst+1
                }
                case _ =>{}
            }
        }
        sourse.close()
        val consts: Array[String]=new Array[String](Numconst)
        val Nameconsts: Array[String]=new Array[String](Numconst)
        for(i<- 0 to Numconst-1){
            consts(i)="0"
        }
        Numconst=0
        println("/////////////////////////////scanning const//////////////////////")
        sourse=Source.fromFile(fname)
        lineIterator=sourse.getLines()
        for(line<-lineIterator){
            // println("current lines, ", line)
            line match {
                case constPatternF(constName, constNum) =>{
                    consts(Numconst)=constNum
                    Nameconsts(Numconst)=constName
                    Numconst=Numconst+1
                    // println("get const ",constName," with num ",constNum)
                }
                case _ =>{}
            }
        }
        sourse.close()
        var startNum:Int=0
        var endNum:Int=0
        sourse=Source.fromFile(fname)
        lineIterator=sourse.getLines()
        println("/////////////////////////////scanning reverse//////////////////////")
        for(line<-lineIterator){
            // println("current lines, ", line)
            line match {
                case pattern3(a0, a1,a2,a3) =>{
                    endNum=pt.getPENum(PEarrName,opNum,a1)
                    if(a3.compareTo("1")==0){
                        PEarrPredR(endNum)=1
                        // println("get reverse ")
                    }
                }
                case _ =>{}
            }
        }
        sourse.close()
        //parse connection
        sourse=Source.fromFile(fname)
        lineIterator=sourse.getLines()
        var constEdge:Int=0
        println("/////////////////////////////scanning edges//////////////////////")
        for(line<-lineIterator){
            // println("current lines, ", line)
            line match {
                
                case pattern2(startEdge, destEdge, operands) =>{
                    // println("current scanning ",line)
                    startEdge match{
                        case cp(con)=>{constEdge=constEdge+1}//never process const edge
                        case _ => {
                            startNum=pt.getPENum(PEarrName,opNum,startEdge)
                            endNum=pt.getPENum(PEarrName,opNum,destEdge)
                            println("Find ", startEdge," at ",startNum.toString(), " to ",destEdge," at ",endNum.toString()," with operand ",operands)
                            
                            if(pt.isPred(PEarrType(endNum),operands)==1){
                                PEarrInPred(endNum)=PEarrInPred(endNum)+1//inc the pred number
                                
                                PEarrOutPredPE(startNum)(PEarrOutPred(startNum))=endNum
                                PEarrOutPredPENum(startNum)(PEarrOutPred(startNum))=1
                                PEarrOutPred(startNum)=PEarrOutPred(startNum)+1
                                //connect the port pred0
                                
                                // println(" this is pred0 edge")
                            }
                            else if(pt.isPred(PEarrType(endNum),operands)==2){
                                PEarrInPred(endNum)=PEarrInPred(endNum)+1
                                PEarrOutPredPE(startNum)(PEarrOutPred(startNum))=endNum
                                PEarrOutPredPENum(startNum)(PEarrOutPred(startNum))=2
                                PEarrOutPred(startNum)=PEarrOutPred(startNum)+1
                                //connect the port pred1
                                
                                // println(" this is pred1 edge")
                            }
                            else{
                                PEarrOutDataPE(startNum)(PEarrOutData(startNum))=endNum
                                
                                PEarrInData(endNum)=PEarrInData(endNum)+1
                                //connect the port data
                                if(pt.isData(PEarrType(endNum),operands)==1){
                                    //port data0
                                    PEarrOutDataPENum(startNum)(PEarrOutData(startNum))=1
                                
                                    // println(" this is data0 edge")
                                }
                                else if(pt.isData(PEarrType(endNum),operands)==2){
                                    //port data1
                                    PEarrOutDataPENum(startNum)(PEarrOutData(startNum))=2
                                    
                                    // println(" this is data1 edge")
                                }
                                PEarrOutData(startNum)=1+PEarrOutData(startNum)
                            }
                        }
                    }
                    
                }
                case _ =>{}
            }
        }
        ///////////////////////////here is a scanning for appending output mux
        var maxPredout=1
        var maxDataout=1
        for(i<-0 to opNum-1){
            if(maxPredout<PEarrOutPred(i)){
                maxPredout=PEarrOutPred(i)
            }
            if(maxDataout<PEarrOutData(i)){
                maxDataout=PEarrOutData(i)
            }
        }
        
        ///////////////////////////end of appending output mux
        println("Const edges ", constEdge.toString())
        println("total memory ops ", LSNum.toString())
        println("total nodes ",(opNum).toString())
        var SeeFloat:Int=0
        sourse=Source.fromFile(fname)
        //parse constant connection
        lineIterator=sourse.getLines()
        for(line<-lineIterator){
            line match {
                case pattern2(startEdge, destEdge, operands) =>{
                    startEdge match{
                        //process const edge
                        case cp(con)=>{
                            startNum=pt.getPENum(Nameconsts,Numconst,startEdge)
                            endNum=pt.getPENum(PEarrName,opNum,destEdge)
                            if(pt.isPred(PEarrType(endNum),operands)==1){
                                PEarrInPred(endNum)=PEarrInPred(endNum)+1//inc the pred number
                            
                                //connect the port pred0
                                
                                if(PEarrType(endNum).compareTo("controlphi")==0){
                                    PEInstArr(endNum)(15)=0
                                    PEInstArr(endNum)(14)=0
                                    PEInstArr(endNum)(13)=1
                                }
                                else if(PEarrType(endNum).compareTo("controlphiLoop")==0){
                                    PEInstArr(endNum)(15)=0
                                    PEInstArr(endNum)(14)=1
                                    PEInstArr(endNum)(13)=1
                                }
                            }
                            else if(pt.isPred(PEarrType(endNum),operands)==2){
                                PEarrInPred(endNum)=PEarrInPred(endNum)+1
                                
                                //connect the port pred1
                                
                                if(PEarrType(endNum).compareTo("controlphi")==0){
                                    PEInstArr(endNum)(15)=0
                                    PEInstArr(endNum)(14)=1
                                    PEInstArr(endNum)(13)=0
                                }
                            }
                            else{
                                
                                PEarrInData(endNum)=PEarrInData(endNum)+1
                                //connect the port data
                                if(pt.isData(PEarrType(endNum),operands)==1){
                                    //port data0
                                    
                                    //set Imm 
                                    SeeFloat=0
                                    for(uuk<-0 to consts(startNum).length-1){
                                        if(consts(startNum)(uuk)=='.'){
                                            SeeFloat=1
                                        }
                                    }
                                    if(SeeFloat==0){
                                        PEarrImm(endNum)=consts(startNum).toInt
                                        println("I got Imm",consts(startNum).toInt,consts(startNum).toInt)
                                        if(consts(startNum)(0)=='-'){
                                            // println("I got Imm",consts(startNum).toInt,consts(startNum).toInt)
                                            
                                        }
                                    }
                                    else{
                                        PEarrImm(endNum)=java.lang.Float.floatToRawIntBits(consts(startNum).toFloat)
                                        println(consts(startNum).toFloat)
                                    }
                                    PEarrImmHas(endNum)=1
                                    if(PEarrType(endNum).compareTo("phiLoop")==0){
                                        PEInstArr(endNum)(15)=1
                                        PEInstArr(endNum)(14)=0
                                        PEInstArr(endNum)(13)=0
                                    }
                                    // else if(PEarrInPred(i)==0&&PEarrInData(i)==0){
                                    //     PEInstArr(endNum)(15)=1
                                    //     PEInstArr(endNum)(14)=1
                                    //     PEInstArr(endNum)(13)=0
                                    // }
                                }
                                else if(pt.isData(PEarrType(endNum),operands)==2){
                                    //port data1
                                    
                                    //set Imm 
                                    SeeFloat=0
                                    for(uuk<-0 to consts(startNum).length-1){
                                        if(consts(startNum)(uuk)=='.'){
                                            SeeFloat=1
                                        }
                                    }
                                    if(SeeFloat==0){
                                        PEarrImm(endNum)=consts(startNum).toInt
                                        println("I got Imm",consts(startNum).toInt,consts(startNum).toInt)
                                        if(consts(startNum)(0)=='-'){
                                            
                                            
                                        }
                                        PEarrImmHas(endNum)=2
                                    }
                                    else{
                                        PEarrImm(endNum)=java.lang.Float.floatToRawIntBits(consts(startNum).toFloat)
                                        PEarrImmHas(endNum)=2
                                        println(consts(startNum).toFloat)
                                    }
                                }
                            }
                        }
                        case _ =>{}
                    }
                }
                case _ =>{}
            }
        }
        sourse.close()
        //sweep up the PE connections,scan the PE type and choose correct Imm, InstType, data in, pred in, and data/pred out
        for(i<-0 to opNum-1){
            //pred in
            if(PEarrInPred(i)==0&&PEarrInData(i)!=0){
                PEInstArr(i)(1)=0
                PEInstArr(i)(0)=0
            }
            else if(PEarrInPred(i)==1 && PEarrPredR(i)==0){
                PEInstArr(i)(1)=0
                PEInstArr(i)(0)=1
            }
            else if(PEarrInPred(i)==2){
                PEInstArr(i)(1)=1
                PEInstArr(i)(0)=0
            }
            else if(PEarrInPred(i)==1 && PEarrPredR(i)==1){
                PEInstArr(i)(1)=1
                PEInstArr(i)(0)=1
            }
            else{
                println("////////////////////////////////////////////////")
                println("ERROR AT PREDIN at node ", PEarrInPred(i).toString())
                println(PEarrPredR(i).toString())
                println(PEarrType(i))
                println(PEarrName(i))
                println("////////////////////////////////////////////////")
            }
            //data in
            if(PEarrInData(i)==0&&PEarrInPred(i)!=0){
                PEInstArr(i)(6)=0
                PEInstArr(i)(5)=0
                PEInstArr(i)(4)=0
            }
            else if(PEarrInData(i)==1&&PEarrImmHas(i)==0){
                PEInstArr(i)(6)=0
                PEInstArr(i)(5)=0
                PEInstArr(i)(4)=1
            }
            else if(PEarrInData(i)==2&&PEarrImmHas(i)==0){
                PEInstArr(i)(6)=0
                PEInstArr(i)(5)=1
                PEInstArr(i)(4)=0
            }
            else if(PEarrInData(i)==2&&PEarrImmHas(i)==1){
                PEInstArr(i)(6)=1
                PEInstArr(i)(5)=0
                PEInstArr(i)(4)=1
            }
            else if(PEarrInData(i)==2&&PEarrImmHas(i)==2){
                PEInstArr(i)(6)=0
                PEInstArr(i)(5)=1
                PEInstArr(i)(4)=1
            }
            else if(PEarrInData(i)==1&&PEarrImmHas(i)==1){
                PEInstArr(i)(6)=1
                PEInstArr(i)(5)=1
                PEInstArr(i)(4)=0
            }
            else{
                println("////////////////////////////////////////////////")
                println("ERROR AT DATAIN ", PEarrInData(i).toString())
                println(PEarrImmHas(i).toString())
                println(PEarrType(i))
                println(PEarrName(i))
                println("////////////////////////////////////////////////")
            }
            //data/pred out
            if(PEarrOutPred(i)>0&&PEarrOutData(i)>0){
                PEInstArr(i)(3)=1
                PEInstArr(i)(2)=1
            }
            else if(PEarrOutPred(i)>0){
                PEInstArr(i)(3)=1
                PEInstArr(i)(2)=0
            }
            else if(PEarrOutData(i)>0){
                PEInstArr(i)(3)=0
                PEInstArr(i)(2)=1
            }
            else{
                PEInstArr(i)(3)=0
                PEInstArr(i)(2)=0
            }
            //Instruction type
            if(PEarrType(i).compareTo("add")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("sub")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("eq")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("lt")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("gt")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("uneq")==0){
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("leq")==0){
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("geq")==0){
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("sleq")==0){
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("sgeq")==0){
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("band")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("bor")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("bshl")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("bshr")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("bshrl")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("bxor")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("mul")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("udiv")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("div")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("urem")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("rem")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("gen")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("loopout")==0){
                if(PEarrOutData(i)>0){
                    PEInstArr(i)(12)=0
                    PEInstArr(i)(11)=1
                    PEInstArr(i)(10)=0
                    PEInstArr(i)(9)=0
                    PEInstArr(i)(8)=1
                    PEInstArr(i)(7)=0
                }
                else{
                    PEInstArr(i)(12)=1
                    PEInstArr(i)(11)=0
                    PEInstArr(i)(10)=0
                    PEInstArr(i)(9)=1
                    PEInstArr(i)(8)=0
                    PEInstArr(i)(7)=1
                }
            }
            else if(PEarrType(i).compareTo("slt")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("sgt")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("fmul")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("fadd")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("fdiv")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("fsub")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("fptosi")==0||PEarrType(i).compareTo("fptoui")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("sitofp")==0||PEarrType(i).compareTo("uitofp")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("loadi")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("storei")==0){
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("phi")==0){
                
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("phiLoop")==0){
                
                PEInstArr(i)(12)=0
                PEInstArr(i)(11)=1
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("controlphi")==0){
                
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=0
                PEInstArr(i)(8)=1
                PEInstArr(i)(7)=1
            }
            else if(PEarrType(i).compareTo("controlphiLoop")==0){
                
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=0
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else if(PEarrType(i).compareTo("mergepred")==0){
                
                PEInstArr(i)(12)=1
                PEInstArr(i)(11)=0
                PEInstArr(i)(10)=1
                PEInstArr(i)(9)=1
                PEInstArr(i)(8)=0
                PEInstArr(i)(7)=0
            }
            else{
                println("ERROR AT INSTTYPE")
            }
            //Imm
            // for(j<-0 to dataWidth-1){
            //     PEInstArr(i)(j+dataWidth)=(PEarrImm(i).toBinaryString)(j)-48
            // }
            // println(PEarrImm(0))
            // println(PEarrImm(1))
            for (j <- 0 until dataWidth) {
                val binaryStr = PEarrImm(i).toBinaryString.reverse.padTo(dataWidth, '0')
                PEInstArr(i)(j + dataWidth) = binaryStr(j) - '0'
            }
            
            //attatch inst    
            // PEarr(i).io.instruction_in.bits=PEInstArr(i).asUInt
            // PEarr(i).io.instruction_in.valid=io.instruction_in_valid
            // rerun(i)=PEarr(i).io.rerun
            // PEarr(i).io.start=io.start
            

        }
        // println(PEarrImm(1))
        // PEInstArr(1).slice(32, 64).foreach(println)

        assert(opNum<=this.CGRASize*this.perInst)
        for(i<- 0 to opNum-1){
            for(j<- 0 to 2*dataWidth-1){
                this.originalInstArray(i)(j)=PEInstArr(i)(j)
            }
        }

        val highBitsArray: Array[Int] = originalInstArray.map { row =>
            var highBits = 0
            for (j <- 32 until 64 reverse) {
                highBits = (highBits << 1) | row(j)
            }
            highBits
        }

        highBitsArray.zipWithIndex.take(opNum).foreach { case (value, i) =>
        println(s"originalInstArray($i) high 32 bits as decimal = $value")
        }
        for(i<- 0 to this.maxInst-1){
            if(i<opNum){
                this.connectPredNum(i)=PEarrOutPred(i)
                this.connectDataNum(i)=PEarrOutData(i)
                for(j<-0 to this.maxInst-1){
                    if(j<opNum){
                        this.OutPredPE(i)(j)=PEarrOutPredPE(i)(j)
                        this.OutDataPE(i)(j)=PEarrOutDataPE(i)(j)
                        this.OutPredPENum(i)(j)=PEarrOutPredPENum(i)(j)
                        this.OutDataPENum(i)(j)=PEarrOutDataPENum(i)(j)
                    }
                    else{
                        this.OutPredPE(i)(j)= 0
                        this.OutDataPE(i)(j)= 0
                        this.OutPredPENum(i)(j)= 0
                        this.OutDataPENum(i)(j)= 0
                    }
                }
            }
            else{
                this.connectPredNum(i)= 0
                this.connectDataNum(i)= 0
                for(j<-0 to this.maxInst-1){
                    
                    this.OutPredPE(i)(j)= 0
                    this.OutDataPE(i)(j)= 0
                    this.OutPredPENum(i)(j)= 0
                    this.OutDataPENum(i)(j)= 0
                }
            }
        }
        this.appSize=opNum
        return 1
    }

    def analyzeBackendPass0():Int = {//resolve routing nodes
        //analyze the original edges and the fanout edges, get which nodes are splitted into several routing nodes
        //for each node which is splitted by the fanout pass, identify the pred and data out of each newly-added routing node

        //1. compare the original edges and the fanout edges
        var node2besplitted: Array[Int] = new Array[Int](this.maxInst)
        var splitNum: Array[Int] = new Array[Int](this.maxInst)
        for(i<- 0 to this.maxInst-1){
            node2besplitted(i)= -1
            splitNum(i)=0
        }
        var PEOutConnection:Array[Array[Int]]= new Array[Array[Int]](this.maxInst)
        for(i<- 0 to this.maxInst-1){
            PEOutConnection(i)=new Array[Int](this.maxInst)
            for(j<- 0 to this.maxInst-1){
                PEOutConnection(i)(j)=0
            }
        }
        //scanning the edges of tagconnections
        this.tagConnections.zipWithIndex.foreach { case (value, index) =>
            {
                // println(s"tagConnections($index) = [${value.mkString(", ")}]")
                // val (src, dst) = value
                var src0=value(0)
                val dst0=value(1)
                if(src0<this.maxInst && dst0<this.maxInst){
                    PEOutConnection(src0)(dst0)=1
                }
            }
        
        }
        //scanning the fanoutTagArray and decide which one is the node splitted
        var maxNumber=0
        this.originalMappingTags.zipWithIndex
        .foreach { case (value, index) =>
            
            {
                if(maxNumber<=value){
                    maxNumber=value
                }
            }
        }
        println(s"maxNumber = $maxNumber")
        var lastFind=0
        var splittedIter=0
        var tmpsplit=0
        this.fanoutMappingTags.zipWithIndex
        .foreach { case (value, index) =>
            
            {
                if(lastFind==0&&value>maxNumber){
                    assert(index>0)
                    node2besplitted(splittedIter)=index-1
                    splitNum(splittedIter)=splitNum(splittedIter)+1
                    tmpsplit=splittedIter
                    lastFind=1
                    splittedIter=splittedIter+1
                }
                else if(lastFind==1&&value>maxNumber){
                    splitNum(tmpsplit)=splitNum(tmpsplit)+1
                }
                else if(value<=maxNumber){
                    lastFind=0
                    
                }
            }
        }
        //get splitted nodes' original edges, only need to scan the last routing nodes and adjust its output modes
        for(i<- 0 to splittedIter-1){
            var splittedNode=node2besplitted(i)+splitNum(i)//get the last routing node
            
            var nodeNUM=fanoutMappingTags(node2besplitted(i))
            var prt=fanoutMappingTags(splittedNode)
            
            var originalNode=0
            this.originalMappingTags.zipWithIndex
            .foreach { case (value, index) =>
                
                {
                    if(value==nodeNUM){
                        originalNode=index//get the original splitted node
                    }
                }
            }
            println(s"Splitting last one $prt, while original is $nodeNUM, at $originalNode")
            var stillKeepData=0
            var stillKeepPred=0
            var connectedNode=0
            var originalData=0
            var originalPred=0
            for(k<-0 to this.connectDataNum(originalNode)-1){
                if(this.OutDataPENum(originalNode)(k)!=0){
                    //identical data edges
                    originalData=1
                }
            }
            for(k<-0 to this.connectPredNum(originalNode)-1){
                if(this.OutPredPENum(originalNode)(k)!=0){
                    //identical pred edges
                    originalPred=1
                }
            }
            assert(originalData==1||originalPred==1)
            //scan the difference between original edges and current edges
            for(j<- 0 to this.fanoutMappingTags.length-1){
                connectedNode=this.fanoutMappingTags(j)
                
                if(PEOutConnection(splittedNode)(j)==1){
                    var myfind=0
                    for(k<-0 to this.connectDataNum(originalNode)-1){
                        if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))){
                            stillKeepData=1
                            myfind=1
                        }
                        
                    }
                    for(k<-0 to this.connectPredNum(originalNode)-1){
                        if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))){
                            stillKeepPred=1
                            myfind=1
                        }
                        
                    }
                    assert(myfind==1)
                }
            }
            //adjust the routing node's instruction
            for(m<- 0 to splitNum(i)-1){
                if(originalPred==1){//routing should have predin
                    this.fanoutInstArray(node2besplitted(i)+1+m)(0)=1
                    this.fanoutInstArray(node2besplitted(i)+1+m)(1)=0
                }
                else{
                    this.fanoutInstArray(node2besplitted(i)+1+m)(0)=0
                    this.fanoutInstArray(node2besplitted(i)+1+m)(1)=0
                }
                if(originalData==1){//routing should have datain
                    this.fanoutInstArray(node2besplitted(i)+1+m)(4)=1
                    this.fanoutInstArray(node2besplitted(i)+1+m)(5)=0
                    this.fanoutInstArray(node2besplitted(i)+1+m)(6)=0
                }
                else{
                    this.fanoutInstArray(node2besplitted(i)+1+m)(4)=0
                    this.fanoutInstArray(node2besplitted(i)+1+m)(5)=1
                    this.fanoutInstArray(node2besplitted(i)+1+m)(6)=1
                }

                if(originalPred==1&&originalData==1){//have both predout and data out
                    this.fanoutInstArray(node2besplitted(i)+1+m)(2)=1
                    this.fanoutInstArray(node2besplitted(i)+1+m)(3)=1
                }
                else if(originalPred==1){
                    this.fanoutInstArray(node2besplitted(i)+1+m)(2)=0
                    this.fanoutInstArray(node2besplitted(i)+1+m)(3)=1
                }
                else if(originalData==1){
                    this.fanoutInstArray(node2besplitted(i)+1+m)(2)=1
                    this.fanoutInstArray(node2besplitted(i)+1+m)(3)=0
                }
                else{
                    println("Routing node error")
                    assert(1==0)
                }

                
                if(m==splitNum(i)-1){//special adjustment for the last routing node
                    if(stillKeepData!=originalData && stillKeepPred==originalPred){//only predout
                        this.fanoutInstArray(node2besplitted(i)+1+m)(2)=0
                        this.fanoutInstArray(node2besplitted(i)+1+m)(3)=1
                    }
                    else if(stillKeepPred!=originalPred && stillKeepData==originalData){//only data out
                        this.fanoutInstArray(node2besplitted(i)+1+m)(2)=1
                        this.fanoutInstArray(node2besplitted(i)+1+m)(3)=0
                    }
                    else if(stillKeepPred==originalPred && stillKeepData==originalData){

                    }
                    else{
                        println("Last routing node error")
                        assert(1==0)
                    }
                }
                else{
                    // var needSplitPred=0
                    // var needSplitData=0
                    // for(j<- 0 to this.fanoutMappingTags.length-1){
                    //     connectedNode=this.fanoutMappingTags(j)
                        
                    //     if(PEOutConnection(node2besplitted(i)+1+m)(j)==1){
                    //         var myfind=0
                    //         for(k<-0 to this.connectDataNum(originalNode)-1){
                    //             if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))){
                    //                 needSplitData=1
                    //                 myfind=1
                    //             }
                                
                    //         }
                    //         for(k<-0 to this.connectPredNum(originalNode)-1){
                    //             if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))){
                    //                 needSplitPred=1
                    //                 myfind=1
                    //             }
                                
                    //         }
                    //         assert(myfind==1)
                    //     }
                    // }
                    // if(needSplitData!=originalData && needSplitPred==originalPred){//only predout
                    //     this.fanoutInstArray(node2besplitted(i)+1+m)(2)=0
                    //     this.fanoutInstArray(node2besplitted(i)+1+m)(3)=1
                    // }
                    // else if(needSplitPred!=originalPred && needSplitData==originalData){//only data out
                    //     this.fanoutInstArray(node2besplitted(i)+1+m)(2)=1
                    //     this.fanoutInstArray(node2besplitted(i)+1+m)(3)=0
                    // }
                    // else if(needSplitPred==originalPred && needSplitData==originalData){

                    // }
                    // else{
                    //     println("Mediate routing node error")
                    //     assert(1==0)
                    // }
                }
                this.fanoutInstArray(node2besplitted(i)+1+m)(7)=1
                this.fanoutInstArray(node2besplitted(i)+1+m)(8)=1
                this.fanoutInstArray(node2besplitted(i)+1+m)(9)=0
                this.fanoutInstArray(node2besplitted(i)+1+m)(10)=1
                this.fanoutInstArray(node2besplitted(i)+1+m)(11)=0
                this.fanoutInstArray(node2besplitted(i)+1+m)(12)=1

                this.fanoutInstArray(node2besplitted(i)+1+m)(13)=0
                this.fanoutInstArray(node2besplitted(i)+1+m)(14)=0
                this.fanoutInstArray(node2besplitted(i)+1+m)(15)=0
            }
            
            
        }
        this.fanoutInstArray.zipWithIndex.take(this.fanoutMappingTags.length).foreach { case (value, index) =>
            println(s"fanoutInstArray($index) = ${value.mkString(",")}")
        }
        return 0
    }

    def analyzeBackendPass1():Int = {
        //analyze the connection path and add the extra routing insts
        //for those paths which contain more than two PE, distinguish how many routing insts should be added and how they connects
        var myNumerator:Array[Int] = new Array[Int](this.connectPathsLengths.length)
        for(i<- 0 to this.maxInst-1){
            for(j <- 0 until this.dataWidth*2){
                this.finalInstArray(i)(j)=0
            }
            // myNumerator(i)=0
        }
        for(i<- 0 to this.connectPathsLengths.length-1){
            // for(j <- 0 until this.dataWidth*2){
            //     this.finalInstArray(i)(j)=0
            // }
            myNumerator(i)=0
        }
        this.fanoutInstArray.zipWithIndex.take(this.fanoutMappingTags.length).foreach { case (value, index) =>
            for(i <- 0 until this.dataWidth*2){
                this.finalInstArray(index)(i)=this.fanoutInstArray(index)(i)
            }
        }
        this.totalInstNum=this.fanoutMappingTags.length
        println(s"totalInstNum = ${this.totalInstNum}")

        this.placements.zipWithIndex.foreach { case (value, index) =>
            this.finalplacements(index)=value
        }

        var PEOutConnection:Array[Array[Int]]= new Array[Array[Int]](this.maxInst)
        var PEOutConnectionbk:Array[Array[Int]]= new Array[Array[Int]](this.maxInst)
        for(i<- 0 to this.maxInst-1){
            PEOutConnection(i)=new Array[Int](this.maxInst)
            PEOutConnectionbk(i)=new Array[Int](this.maxInst)
            this.finalConnection(i)=new Array[Int](this.maxInst)
            this.oriConnection(i)=new Array[Int](this.maxInst)
            this.finalConnectionbk(i)=new Array[Int](this.maxInst)
            for(j<- 0 to this.maxInst-1){
                PEOutConnection(i)(j)=0
                PEOutConnectionbk(i)(j)=0
                this.finalConnection(i)(j)=0
                this.oriConnection(i)(j)=0
                this.finalConnectionbk(i)(j)=0
            }
        }
        //scanning the edges of tagconnections
        this.tagConnections.zipWithIndex.foreach { case (value, index) =>
            {
                
                var src0=value(0)
                val dst0=value(1)
                if(src0<this.maxInst && dst0<this.maxInst){
                    PEOutConnection(src0)(dst0)=1
                    PEOutConnectionbk(src0)(dst0)=1
                    var srcRoute=1
                    for(k<- 0 to this.originalMappingTags.length-1){
                        if(this.originalMappingTags(k)==this.fanoutMappingTags(src0)){
                            srcRoute=0
                        }
                    }
                    if(srcRoute==0){

                        this.oriConnection(src0)(dst0)=1
                    }
                    else{
                        var orz=0
                        var findo=0
                        var originalNode=src0
                        while(findo==0){
                            originalNode=originalNode-1
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                    findo=1
                                }
                            }
                            
                        }
                        assert(findo==1&&originalNode>=0)
                        this.oriConnection(originalNode)(dst0)=1
                    }
                }
            }
        
        }
        
        var iter=0
        for(i<-0 to this.connectPathsLengths.length-1){
            if(this.connectPathsLengths(i)>2){
                //for those paths which contain more than two PEs, first capture their starts, check if the starts are the inst's mapped PE
                //if it is, then new the routing insts and add corresponding edges, if not, also new the routing insts, but need to find out the inst connections
                var toadds=this.connectPathsLengths(i)-2
                var mystart=this.connectPaths(i)(0)
                var correspondingInst=this.tagConnections(i)(0)
                var correspondingPE=this.placements(correspondingInst)

                var lastInst=this.tagConnections(i)(0)
                //pop the corresponding connections from the PEOutConnection matrix first
                
                // PEOutConnectionbk(this.tagConnections(i)(0))(this.tagConnections(i)(1))=0
                //create new Inst and add connections
                if(correspondingPE==mystart){
                    var myend=this.connectPaths(i)(this.connectPathsLengths(i)-2)
                    // println(s"myend1 is $myend")
                    myNumerator(i)=this.totalInstNum
                    PEOutConnection(this.tagConnections(i)(0))(this.tagConnections(i)(1))=0
                    for(j<- 0 to toadds-1){
                        PEOutConnection(lastInst)(this.totalInstNum)=1
                        PEOutConnectionbk(this.tagConnections(i)(0))(this.totalInstNum)=1
                        // var pn=this.tagConnections(i)(0)
                        // var np=this.totalInstNum
                        // println(s"for node $pn, we add a connection to $np")
                        lastInst=this.totalInstNum
                        
                        this.finalplacements(this.totalInstNum)=this.connectPaths(i)(1+j)
                        this.totalInstNum=this.totalInstNum+1
                        
                    }
                    PEOutConnection(lastInst)(this.tagConnections(i)(1))=1
                    // PEOutConnectionbk(this.tagConnections(i)(0))(this.totalInstNum)=1
                }
                
            }
        }
        var changes=1
        var scanned:Array[Int] = new Array[Int](this.maxEdges)
        for(i <- 0 until this.maxEdges){
            scanned(i)=0
        }
        while(changes==1){
            changes=0
            for(i<-0 to this.connectPathsLengths.length-1){
                if(this.connectPathsLengths(i)>=2){
                    //for those paths which contain more than two PEs, first capture their starts, check if the starts are the inst's mapped PE
                    //if it is, then new the routing insts and add corresponding edges, if not, also new the routing insts, but need to find out the inst connections
                    var toadds=this.connectPathsLengths(i)-2
                    var mystart=this.connectPaths(i)(0)
                    var correspondingInst=this.tagConnections(i)(0)
                    var correspondingPE=this.placements(correspondingInst)

                    var lastInst=this.tagConnections(i)(0)
                    var ending=this.tagConnections(i)(1)
                    //pop the corresponding connections from the PEOutConnection matrix first
                    
                    //create new Inst and add connections
                    
                    if(correspondingPE!=mystart){
                        // println(s"Corresponding PE: $correspondingPE, mystart $mystart, correspondingInst $correspondingInst, lastInst $ending")
                        // println("//////////////////////////////////////////////////////////////////////////////")
                        //first find that corresponding start inst
                        var myInst=0
                        var mype=0
                        var kfind=0
                        for(j<-this.fanoutMappingTags.length to this.totalInstNum-1){
                            if(PEOutConnectionbk(this.tagConnections(i)(0))(j)==1&&this.finalplacements(j)==mystart){
                                myInst=j
                                mype=mystart
                                kfind=1
                            }
                        }
                        if(kfind==0){
                            println(s"Corresponding PE: $correspondingPE, mystart $mystart, correspondingInst $correspondingInst, lastInst $ending")
                        }
                        assert(kfind==1)
                        if(kfind==1&&scanned(i)==0){
                            scanned(i)=1
                            assert(kfind==1)
                            lastInst=myInst
                            myNumerator(i)=this.totalInstNum
                            PEOutConnection(this.tagConnections(i)(0))(this.tagConnections(i)(1))=0
                            for(j<- 0 to toadds-1){
                                PEOutConnection(lastInst)(this.totalInstNum)=1
                                PEOutConnectionbk(this.tagConnections(i)(0))(this.totalInstNum)=1
                                lastInst=this.totalInstNum
                                
                                
                                this.finalplacements(this.totalInstNum)=this.connectPaths(i)(1+j)
                                this.totalInstNum=this.totalInstNum+1
                                
                            }
                            PEOutConnection(lastInst)(this.tagConnections(i)(1))=1
                            changes=1
                        }
                    }
                }
            }
        }
        //now connections and routing insts are ready, it is now to construct the instcode
        //distinguish each routing's pred/data in and pred/data out
        //for the immediate routing, its pred/data out is set the same as original insts, only the last should be different
        var tmp_it=0
        var tmp_total=this.fanoutMappingTags.length
        for(i<-0 to this.connectPathsLengths.length-1){
            if(this.connectPathsLengths(i)>2){
                //for those paths which contain more than two PEs, first capture their starts, check if the starts are the inst's mapped PE
                //then perform the construction for each inst
                var toadds0=this.connectPathsLengths(i)-2
                var mystart0=this.connectPaths(i)(0)
                var correspondingInst0=this.tagConnections(i)(0)
                var correspondingPE0=this.placements(correspondingInst0)

                // var myend=this.connectPaths(i)(this.connectPathsLengths(i)-2)
                // println(s"myend2 is $myend")
                //create new Inst
                if(correspondingPE0==mystart0){
                    var myend=this.connectPaths(i)(this.connectPathsLengths(i)-2)
                    tmp_total=myNumerator(i)
                    // println(s"myend2 is $myend")
                    for(j<- 0 to toadds0-1){
                        //set output
                        this.finalInstArray(tmp_total)(2)=this.finalInstArray(correspondingInst0)(2)
                        this.finalInstArray(tmp_total)(3)=this.finalInstArray(correspondingInst0)(3)
                        //set predin
                        if(this.finalInstArray(correspondingInst0)(3)==1){
                            this.finalInstArray(tmp_total)(0)=1
                            this.finalInstArray(tmp_total)(1)=0
                        }
                        else{
                            this.finalInstArray(tmp_total)(0)=0
                            this.finalInstArray(tmp_total)(1)=0
                        }
                        //set data in
                        if(this.finalInstArray(correspondingInst0)(2)==1){
                            this.finalInstArray(tmp_total)(4)=1
                            this.finalInstArray(tmp_total)(5)=0
                            this.finalInstArray(tmp_total)(6)=0
                        }
                        else{
                            this.finalInstArray(tmp_total)(4)=0
                            this.finalInstArray(tmp_total)(5)=1
                            this.finalInstArray(tmp_total)(6)=1
                        }
                        //set inst type
                        this.finalInstArray(tmp_total)(7)=1
                        this.finalInstArray(tmp_total)(8)=1
                        this.finalInstArray(tmp_total)(9)=0
                        this.finalInstArray(tmp_total)(10)=1
                        this.finalInstArray(tmp_total)(11)=0
                        this.finalInstArray(tmp_total)(12)=1

                        this.finalInstArray(tmp_total)(13)=0
                        this.finalInstArray(tmp_total)(14)=0
                        this.finalInstArray(tmp_total)(15)=0
                        tmp_total=tmp_total+1
                        
                    }
                    
                }
                
            }
        }

        for(i<-0 to this.connectPathsLengths.length-1){
            if(this.connectPathsLengths(i)>=2){
                //for those paths which contain more than two PEs, first capture their starts, check if the starts are the inst's mapped PE
                //if it is, then new the routing insts and add corresponding edges, if not, also new the routing insts, but need to find out the inst connections
                var toadds=this.connectPathsLengths(i)-2
                var mystart=this.connectPaths(i)(0)
                var correspondingInst=this.tagConnections(i)(0)
                var correspondingPE=this.placements(correspondingInst)
                var myend=this.connectPaths(i)(this.connectPathsLengths(i)-2)
                // println(s"myend2 is $myend")
                
                //create new Inst and add connections
                
                if(correspondingPE!=mystart){
                    //first find that corresponding start inst
                    var myInst=0
                    var mype=0
                    var kfind=0
                    for(j<-this.fanoutMappingTags.length to this.totalInstNum-1){
                        if(PEOutConnectionbk(this.tagConnections(i)(0))(j)==1&&this.finalplacements(j)==mystart){
                            myInst=j
                            mype=mystart
                            kfind=1
                        }
                    }
                    assert(kfind==1)
                    tmp_total=myNumerator(i)
                    // lastInst=myInst
                    for(j<- 0 to toadds-1){
                        this.finalInstArray(tmp_total)(2)=this.finalInstArray(this.tagConnections(i)(0))(2)
                        this.finalInstArray(tmp_total)(3)=this.finalInstArray(this.tagConnections(i)(0))(3)
                        //set predin
                        if(this.finalInstArray(this.tagConnections(i)(0))(3)==1){
                            this.finalInstArray(tmp_total)(0)=1
                            this.finalInstArray(tmp_total)(1)=0
                        }
                        else{
                            this.finalInstArray(tmp_total)(0)=0
                            this.finalInstArray(tmp_total)(1)=0
                        }
                        //set data in
                        if(this.finalInstArray(this.tagConnections(i)(0))(2)==1){
                            this.finalInstArray(tmp_total)(4)=1
                            this.finalInstArray(tmp_total)(5)=0
                            this.finalInstArray(tmp_total)(6)=0
                        }
                        else{
                            this.finalInstArray(tmp_total)(4)=0
                            this.finalInstArray(tmp_total)(5)=1
                            this.finalInstArray(tmp_total)(6)=1
                        }
                        //set inst type
                        this.finalInstArray(tmp_total)(7)=1
                        this.finalInstArray(tmp_total)(8)=1
                        this.finalInstArray(tmp_total)(9)=0
                        this.finalInstArray(tmp_total)(10)=1
                        this.finalInstArray(tmp_total)(11)=0
                        this.finalInstArray(tmp_total)(12)=1

                        this.finalInstArray(tmp_total)(13)=0
                        this.finalInstArray(tmp_total)(14)=0
                        this.finalInstArray(tmp_total)(15)=0
                        tmp_total=tmp_total+1
                        
                    }
                    
                }
            }
        }
        //preparation is already done, now revise the output for each last routing inst
        var instKept:Array[Int] =new Array[Int](this.maxInst)
        for(i <- 0 until this.maxInst){
            instKept(i) = 0
        }
        tmp_total=this.fanoutMappingTags.length
        for(i<-0 to this.connectPathsLengths.length-1){
            if(this.connectPathsLengths(i)>=2){
                var toadds=this.connectPathsLengths(i)-2
                var mystart=this.connectPaths(i)(0)
                var myend=this.connectPaths(i)(this.connectPathsLengths(i)-2)
                var destnode=this.connectPaths(i)(this.connectPathsLengths(i)-1)
                var correspondingInst=this.tagConnections(i)(0)
                var correspondingPE=this.placements(correspondingInst)
                // println(s"myend is $myend")
                var correspondingEnd=this.tagConnections(i)(1)

                //finding out the connections for the last routing instruction is pred or data, 
                //special cases which indicate the last routing inst is an immediate routing node in the other path should not be processed
                //first judge myend is in other paths
                //also, the routing instruction should take the cases that two end instructions are in the same path
                if(this.connectPathsLengths(i)>2||mystart!=correspondingPE){
                    var myInst=0
                    var instf=0
                    for(j<-this.fanoutMappingTags.length to this.totalInstNum-1){
                        if(PEOutConnectionbk(this.tagConnections(i)(0))(j)==1){
                            if(this.finalplacements(j)==myend){
                                myInst=j//find end inst
                                instf=1
                            }
                            else{

                            }
                        }
                    }
                    assert(instf==1)
                    var inOtherPath=0
                    for(j<-0 to this.totalInstNum-1){
                        if(PEOutConnection(myInst)(j)==1&&j>=this.fanoutMappingTags.length){
                            inOtherPath=1//connect to other path
                            println("////////////////////////////////////////")
                            println("in other paths")
                        }
                        else if(PEOutConnection(myInst)(j)==1){
                            var isr=1
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(j)){
                                    isr=0
                                }
                            }
                            if(isr==1){//the end is not in other path, and it is not a routing node, need to revise the output
                                inOtherPath=1//connect to other path
                                println("////////////////////////////////////////")
                                println("in other paths")
                            }
                        }
                    }
                    if(inOtherPath==1){
                        //no revisions
                    }
                    else{//the end is safe for revision
                        
                        // first check whether the dest is a routing node
                        var isrouting=1
                        for(k<- 0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(correspondingEnd)){
                                isrouting=0
                            }
                        }
                        //end is route, no need to revise the output
                        if(isrouting==0){
                            isrouting=1
                            var myk=0
                            //check whether the src is a routing node, if it is a routing node, try to find the original node
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(correspondingInst)){
                                    isrouting=0
                                    myk=k
                                }
                            }
                            var originalNode=correspondingInst
                            if(isrouting==0){
                                originalNode=correspondingInst
                            }
                            else{//source back to the original
                                var findo=0
                                while(findo==0){
                                    originalNode=originalNode-1
                                    for(k<- 0 to this.originalMappingTags.length-1){
                                        if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                            findo=1
                                        }
                                    }
                                    
                                }
                                assert(originalNode>=0)
                            }
                            var realOriginal=0
                            var canfind=0
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                    realOriginal=k
                                    canfind=1
                                }
                            }
                            assert(canfind>0)
                            originalNode=realOriginal
                            //now find the exact edge between the original src and dest (all non-routing)
                            var connectedNode=this.fanoutMappingTags(correspondingEnd)
                            var stillKeepData=0
                            var stillKeepPred=0
                            // println(s"original instnum is $correspondingInst while instnum is $correspondingEnd")
                            // println(s"original ndoe is $originalNode while end is $connectedNode")
                            var myfind=0
                            for(k<-0 to this.connectDataNum(originalNode)-1){
                                if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))){
                                    stillKeepData=1
                                    myfind=1
                                }
                                
                            }
                            for(k<-0 to this.connectPredNum(originalNode)-1){
                                if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))){
                                    stillKeepPred=1
                                    myfind=1
                                }
                                
                            }
                            assert(myfind==1)
                            //now it is time for revision
                            if(stillKeepPred==1&&stillKeepData==1){
                                this.finalInstArray(myInst)(2)=1
                                this.finalInstArray(myInst)(3)=1
                            }
                            else if(stillKeepPred==1){
                                this.finalInstArray(myInst)(2)=0
                                this.finalInstArray(myInst)(3)=1
                            }
                            else if(stillKeepData==1){
                                this.finalInstArray(myInst)(2)=1
                                this.finalInstArray(myInst)(3)=0
                            }
                            else{
                                assert(false)
                            }
                        }
                    }

                    // tmp_total=tmp_total+1
                }
            }
        }
        //second pass to generate the correct results
        for(i<-0 to this.connectPathsLengths.length-1){
            if(this.connectPathsLengths(i)>=2){
                var toadds=this.connectPathsLengths(i)-2
                var mystart=this.connectPaths(i)(0)
                var myend=this.connectPaths(i)(this.connectPathsLengths(i)-2)
                var destnode=this.connectPaths(i)(this.connectPathsLengths(i)-1)
                var correspondingInst=this.tagConnections(i)(0)
                var correspondingPE=this.placements(correspondingInst)
                // println(s"myend is $myend")
                var correspondingEnd=this.tagConnections(i)(1)

                //finding out the connections for the last routing instruction is pred or data, 
                //special cases which indicate the last routing inst is an immediate routing node in the other path should not be processed
                //first judge myend is in other paths
                //also, the routing instruction should take the cases that two end instructions are in the same path
                if(this.connectPathsLengths(i)>2||mystart!=correspondingPE){
                    var myInst=0
                    var instf=0
                    for(j<-this.fanoutMappingTags.length to this.totalInstNum-1){
                        if(PEOutConnectionbk(this.tagConnections(i)(0))(j)==1){
                            if(this.finalplacements(j)==myend){
                                myInst=j//find end inst
                                instf=1
                            }
                            else{

                            }
                        }
                    }
                    assert(instf==1)
                    var inOtherPath=0
                    for(j<-0 to this.totalInstNum-1){
                        if(PEOutConnection(myInst)(j)==1&&j>=this.fanoutMappingTags.length){
                            inOtherPath=1//connect to other path
                            println("////////////////////////////////////////")
                            println("in other paths")
                        }
                        else if(PEOutConnection(myInst)(j)==1){
                            var isr=1
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(j)){
                                    isr=0
                                }
                            }
                            if(isr==1){//the end is not in other path, and it is not a routing node, need to revise the output
                                inOtherPath=1//connect to other path
                                println("////////////////////////////////////////")
                                println("in other paths")
                            }
                        }
                    }
                    if(inOtherPath==1){
                        //no revisions
                    }
                    else{//the end is safe for revision
                        
                        // first check whether the dest is a routing node
                        var isrouting=1
                        for(k<- 0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(correspondingEnd)){
                                isrouting=0
                            }
                        }
                        //end is route, no need to revise the output
                        if(isrouting==0){
                            isrouting=1
                            var myk=0
                            //check whether the src is a routing node, if it is a routing node, try to find the original node
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(correspondingInst)){
                                    isrouting=0
                                    myk=k
                                }
                            }
                            var originalNode=correspondingInst
                            if(isrouting==0){
                                originalNode=correspondingInst
                            }
                            else{//source back to the original
                                var findo=0
                                while(findo==0){
                                    originalNode=originalNode-1
                                    for(k<- 0 to this.originalMappingTags.length-1){
                                        if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                            findo=1
                                        }
                                    }
                                    
                                }
                                assert(originalNode>=0)
                            }
                            var realOriginal=0
                            var canfind=0
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                    realOriginal=k
                                    canfind=1
                                }
                            }
                            assert(canfind>0)
                            originalNode=realOriginal
                            //now find the exact edge between the original src and dest (all non-routing)
                            var connectedNode=this.fanoutMappingTags(correspondingEnd)
                            var stillKeepData=0
                            var stillKeepPred=0
                            // println(s"original instnum is $correspondingInst while instnum is $correspondingEnd")
                            // println(s"original ndoe is $originalNode while end is $connectedNode")
                            var myfind=0
                            for(k<-0 to this.connectDataNum(originalNode)-1){
                                if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))){
                                    stillKeepData=1
                                    myfind=1
                                }
                                
                            }
                            for(k<-0 to this.connectPredNum(originalNode)-1){
                                if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))){
                                    stillKeepPred=1
                                    myfind=1
                                }
                                
                            }
                            assert(myfind==1)
                            //now it is time for revision
                            if(stillKeepPred==1&&stillKeepData==1){
                                this.finalInstArray(myInst)(2)=1
                                this.finalInstArray(myInst)(3)=1
                            }
                            else if(stillKeepPred==1){
                                // this.finalInstArray(myInst)(2)=0
                                this.finalInstArray(myInst)(3)=1
                            }
                            else if(stillKeepData==1){
                                this.finalInstArray(myInst)(2)=1
                                // this.finalInstArray(myInst)(3)=0
                            }
                            else{
                                assert(false)
                            }
                        }
                    }

                    // tmp_total=tmp_total+1
                }
            }
        }
        //now all the instructions are ready, it is time for the fianl step
        this.finalInstArray.zipWithIndex.take(this.totalInstNum).foreach { case (value, index) =>
            println(s"finalInstArray($index) = ${value.mkString(",")}")
        }
        this.finalplacements.zipWithIndex.take(this.totalInstNum).foreach { case (value, index) =>
            println(s"finalplacements($index) = $value")
        }
        for(i<- 0 to this.maxInst-1){
            
            for(j<- 0 to this.maxInst-1){
                
                
                this.finalConnection(i)(j)=PEOutConnection(i)(j)
                // this.oriConnection(i)(j)=PEOutConnection(i)(j)
                this.finalConnectionbk(i)(j)=PEOutConnectionbk(i)(j)
            }
        }
        // for(i<- 0 to this.totalInstNum-1){
            
        //     print(s"peconnect($i)= ")
        //     for(j<- 0 to this.totalInstNum-1){
                
        //         // var v=PEOutConnection(i)(j)
        //         // println(s"peconnect($i)($j) = $v")
        //         var v=PEOutConnection(i)(j)
        //         print(s"$v, ") 
        //         // this.finalConnectionbk(i)(j)=
        //     }
        //     println()
        // }
        // for(i<- 0 to this.totalInstNum-1){
        //     print(s"peconnectbk($i)= ")
        //     for(j<- 0 to this.totalInstNum-1){
                
        //         // var v=PEOutConnection(i)(j)
        //         // println(s"peconnect($i)($j) = $v")
        //         var v=PEOutConnectionbk(i)(j)
        //         print(s"$v, ") 
        //         // this.finalConnectionbk(i)(j)=
        //     }
        //     println()
        // }
        return 0
    }

    def analyzeBackendPass2(): Int = {
        //set the output configuration here
        //this is the final processing for architectures
        //for each edge, find the src pe and dest pe, deceide the connection is pred/data, then set the outarray
        // this.CGRAGen()
        for(i<-0 to this.maxInst-1){
            this.outSetD0(i)=new Array[Int](this.outSize)
            this.outSetD1(i)=new Array[Int](this.outSize)
            this.outSetP0(i)=new Array[Int](this.outSize)
            this.outSetP1(i)=new Array[Int](this.outSize)
            this.remuoutSetD0(i)=Array.fill(this.outSize)(0)
            this.remuoutSetD1(i)=Array.fill(this.outSize)(0)
            this.remuoutSetD2(i)=Array.fill(this.outSize)(0)
            for(j<- 0 to this.outSize-1){
                this.outSetD0(i)(j)=0
                this.outSetD1(i)(j)=0
                this.outSetP0(i)(j)=0
                this.outSetP1(i)(j)=0
            }

        }
        for(i<-0 to this.totalInstNum-1){
            for(j<- 0 to this.totalInstNum-1){
                if(this.finalConnection(i)(j)==1){
                    var hasSet=0
                    var destPE=this.finalplacements(j)//get dest PE
                    var srcPE=this.finalplacements(i)//get src PE
                    
                    //check which out bit is the dest
                    var numberPE=1
                    var find=0
                    if(destPE==srcPE){
                        numberPE=0
                    }
                    else{
                        var thisf=0
                        for(k<-0 to this.CGRASize-1){
                            if(this.CGRA_array(srcPE)(k)==1&&k!=srcPE){
                                
                                if(k==destPE){
                                    find=1
                                    thisf=1
                                }
                                if(find==0){
                                    numberPE=numberPE+1
                                }
                            }
                        }
                        if(thisf==0){

                            println(s"dest and src is $destPE and $srcPE")
                            println(s"destj and srci is $j and $i")
                        }
                        assert(thisf==1)
                    }
                    var fanoutRouting=1
                    if(j<this.fanoutMappingTags.length){//j belongs the original routed PE
                        for(k<- 0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(j)){
                                fanoutRouting=0
                            }
                        }
                    }
                    //numberPE is the outbit number
                    if(j>=this.fanoutMappingTags.length||fanoutRouting==1){//for routing destination, simply connect the data0 and pred0
                        if(this.finalInstArray(i)(2)==1){
                            this.outSetD0(i)(numberPE)=1
                            hasSet=1
                            this.remuoutSetD0(i)(numberPE)=1
                        }
                        if(this.finalInstArray(i)(3)==1){
                            this.outSetP0(i)(numberPE)=1
                            hasSet=1
                            this.remuoutSetD0(i)(numberPE)=1
                        }
                    }
                    
                    else{//need to traverse the connection to decide which input port should be connected
                        var isrouting=1
                        var correspondingInst=i
                        if(i<this.fanoutMappingTags.length){
                            //check whether the src is a routing node, if it is a routing node, try to find the original node
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(i)){
                                    isrouting=0
                                }
                            }
                        }
                        var originalNode=correspondingInst
                        if(isrouting==0){
                            originalNode=correspondingInst
                        }
                        else{//source back to the original
                            if(i<this.fanoutMappingTags.length){
                                var findo=0
                                while(originalNode-1>=0&&findo==0){
                                    originalNode=originalNode-1
                                    for(k<- 0 to this.originalMappingTags.length-1){
                                        if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                            findo=1
                                        }
                                    }
                                    
                                }
                                assert(originalNode>=0&&findo==1)
                            }
                            else{
                                var myInst=0
                                
                                for(m<- 0 to this.fanoutMappingTags.length-1){
                                    if(this.finalConnectionbk(m)(i)==1){
                                        myInst=m//find src inst
                                    }
                                }
                                var isroutingbk=1
                                //check if it is another routing
                                for(k<- 0 to this.originalMappingTags.length-1){
                                    if(this.originalMappingTags(k)==this.fanoutMappingTags(myInst)){
                                        isroutingbk=0
                                    }
                                }
                                originalNode=myInst
                                
                                if(isroutingbk==1){
                                    // println("Routing for routing")
                                    var findo=0
                                    while(originalNode-1>=0&&findo==0){
                                        originalNode=originalNode-1
                                        for(k<- 0 to this.originalMappingTags.length-1){
                                            if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                                findo=1
                                            }
                                        }
                                        
                                    }
                                    assert(originalNode>=0&&findo==1)
                                }
                                // println(s"Routing $originalNode")
                            }
                        }
                        var realOriginal=0
                        var rf=0
                        for(k<- 0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                realOriginal=k
                                rf=1
                            }
                        }
                        assert(rf==1)
                        originalNode=realOriginal
                        //src is get, check the dest's input
                        //now src and dest are both normal insts
                        var connectedNode=this.fanoutMappingTags(j)
                        var stillKeepData0=0
                        var stillKeepData1=0
                        var stillKeepPred0=0
                        var stillKeepPred1=0
                        //considering src and dest may exist several edges, it should all be found out
                        // println(s"original instnum is $correspondingInst while instnum is $j")
                        // println(s"original ndoe is $originalNode while end is $connectedNode")
                        var myfindd=0
                        var myfindp=0
                        for(k<-0 to this.connectDataNum(originalNode)-1){
                            if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))&&myfindd==0){
                                stillKeepData0=this.OutDataPENum(originalNode)(k)
                                myfindd=1
                            }
                            else if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))&&myfindd==1){
                                stillKeepData1=this.OutDataPENum(originalNode)(k)
                                myfindd=2
                            }
                        }
                        for(k<-0 to this.connectPredNum(originalNode)-1){
                            if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))&&myfindp==0){
                                stillKeepPred0=this.OutPredPENum(originalNode)(k)
                                myfindp=1
                            }
                            else if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))&&myfindp==1){
                                stillKeepPred1=this.OutPredPENum(originalNode)(k)
                                myfindp=2
                            }
                        }
                        assert(myfindd>0||myfindp>0)
                        if(stillKeepData0!=0){
                            if(stillKeepData0==1){
                                this.outSetD0(i)(numberPE)=1
                                // this.remuoutSetD0(i)(numberPE)=1//for data out the temporal and spatial shares the same port
                            }
                            else{
                                this.outSetD1(i)(numberPE)=1
                                // this.remuoutSetD1(i)(numberPE)=1
                            }
                            hasSet=1
                        }
                        if(stillKeepData1!=0){
                            if(stillKeepData1==1){
                                this.outSetD0(i)(numberPE)=1
                                // this.remuoutSetD0(i)(numberPE)=1
                            }
                            else{
                                this.outSetD1(i)(numberPE)=1
                                // this.remuoutSetD1(i)(numberPE)=1
                            }
                            hasSet=1
                        }
                        if(stillKeepPred0!=0){
                            hasSet=1
                            if(stillKeepPred0==1){
                                this.outSetP0(i)(numberPE)=1
                            }
                            else{
                                this.outSetP1(i)(numberPE)=1
                            }
                        }
                        if(stillKeepPred1!=0){
                            hasSet=1
                            if(stillKeepPred1==1){
                                this.outSetP0(i)(numberPE)=1
                            }
                            else{
                                this.outSetP1(i)(numberPE)=1
                            }
                        }
                        assert(hasSet==1)
                    }
                }
            }
        }
        // for(i<-0 to this.totalInstNum-1){
            
        //     println(s"outSetD0: $i " )
        //     for(j<- 0 to this.outSize-1)
        //     {
        //         print(" "+this.outSetD0(i)(j))
        //     }
        //     println()
        //     println(s"outSetD1: $i " )
        //     for(j<- 0 to this.outSize-1)
        //     {
        //         print(" "+this.outSetD1(i)(j))
        //     }
        //     println()
        //     println(s"outSetP0: $i " )
        //     for(j<- 0 to this.outSize-1)
        //     {
        //         print(" "+this.outSetP0(i)(j))
        //     }
        //     println()
        //     println(s"outSetP1: $i " )
        //     for(j<- 0 to this.outSize-1)
        //     {
        //         print(" "+this.outSetP1(i)(j))
        //     }
        //     println()

        // }
        return 0
    }


    def verifications():Int={
        var maxNumber=0
        this.originalMappingTags.zipWithIndex
        .foreach { case (value, index) =>
            
            {
                if(maxNumber<=value){
                    maxNumber=value
                }
            }
        }
        for(i<-0 to this.totalInstNum-1){
            var haspred=this.finalInstArray(i)(3)
            var hasdata=this.finalInstArray(i)(2)
            var hasinpred=0
            var hasindata=0
            for(j<- 0 to this.totalInstNum-1){
                if(this.finalConnection(i)(j)==1){
                    //get the instructions and check the input and output is consistent
                    //get i's out
                    
                    hasinpred=hasinpred|(this.finalInstArray(j)(0)|this.finalInstArray(j)(1))
                    hasindata=hasindata|((this.finalInstArray(j)(4)|this.finalInstArray(j)(5)|this.finalInstArray(j)(6))&(~(this.finalInstArray(j)(5)&this.finalInstArray(j)(6))))
                    
                    // println(s"inst $i has predout $haspred has dataout $hasdata and $j has predin $hasinpred has datain $hasindata")
                    
                }

            }
            if(!((hasdata==1)&&(hasdata==hasindata)||(hasdata==0))){
                if(i<this.fanoutMappingTags.length){
                    println("fanout", this.fanoutMappingTags(i))
                    if(hasdata==1&&maxNumber<this.fanoutMappingTags(i)){
                        hasdata=0
                        this.finalInstArray(i)(2)=0
                    }
                }
                else{
                    // if(hasdata==1){
                    //     hasdata=0
                    //     this.finalInstArray(i)(2)=0
                    // }
                    println("added route", i)
                }
            }
            if(!((haspred==1)&&(haspred==hasinpred)||(haspred==0))){
                if(i<this.fanoutMappingTags.length){
                    println("fanout", this.fanoutMappingTags(i))
                    if(haspred==1&&maxNumber<this.fanoutMappingTags(i)){
                        haspred=0
                        this.finalInstArray(i)(3)=0
                    }
                }
                else{
                    println("added route", i)
                }
            }
            assert((haspred==1)&&(haspred==hasinpred)||(haspred==0))
            assert((hasdata==1)&&(hasdata==hasindata)||(hasdata==0))
            
        }
        for(i<-0 to this.totalInstNum-1){
            var haspred=(this.finalInstArray(i)(0)|this.finalInstArray(i)(1))
            var hasdata=((this.finalInstArray(i)(4)|this.finalInstArray(i)(5)|this.finalInstArray(i)(6))&(~(this.finalInstArray(i)(5)&this.finalInstArray(i)(6))))
                    
            var hasinpred=0
            var hasindata=0
            var findi=0
            for(j<- 0 to this.totalInstNum-1){
                if(this.finalConnection(j)(i)==1){
                    //get the instructions and check the input and output is consistent
                    //get i's out
                    
                    hasinpred=hasinpred|(this.finalInstArray(j)(3))
                    hasindata=hasindata|this.finalInstArray(j)(2)
                    findi=1
                }

            }
            // if(findi!=1){
            //     println(s"find i is $i")
            // }
            // assert(findi==1)
            if(!((haspred==1)&&(haspred==hasinpred)||(haspred==0))){
                println(s"i is $i, haspred is $haspred,hasinpred is $hasinpred")
            }
            if(!((hasdata==1)&&(hasdata==hasindata)||(hasdata==0))){
                println(s"i is $i, hasdata is $hasdata,hasindata is $hasindata")
            }
            assert((haspred==1)&&(haspred==hasinpred)||(haspred==0))
            assert((hasdata==1)&&(hasdata==hasindata)||(hasdata==0))
        }
        for(i<-0 to this.totalInstNum-1){
            
            for(j<- 0 to this.totalInstNum-1){
                if(this.finalConnection(i)(j)==1){
                    //get i's src and j'dest
                    var srcPE=this.finalplacements(i)
                    var destPE=this.finalplacements(j)
                    var pfind=0
                    var pfind1=0
                    var df=0
                    for(k<-0 to this.connectPaths.length-1){
                        pfind=0
                        pfind1=0
                        for(m<- 0 to this.connectPathsLengths(k)-2){
                            if(this.connectPaths(k)(m)==srcPE&&this.connectPaths(k)(m+1)==destPE){
                                pfind=1
                                pfind1=1
                            }
                            if(this.connectPaths(k)(m)==destPE){
                                
                            }
                        }
                        if(pfind==1&&pfind1==1){
                            df=1
                        }
                    }
                    assert(df==1)
                }
            }
        }

        for(k<-0 to this.connectPaths.length-1){
            for(m<- 0 to this.connectPathsLengths(k)-2){
                var src=this.connectPaths(k)(m)
                var dest=this.connectPaths(k)(m+1)
                var pfind=0
                var pfind1=0
                var df=0
                for(i<-0 to this.totalInstNum-1){
                    for(j<- 0 to this.totalInstNum-1){
                        if(this.finalConnection(i)(j)==1){
                            var srcPE=this.finalplacements(i)
                            var destPE=this.finalplacements(j)
                            if(srcPE==src&&destPE==dest){
                                pfind=1
                            }
                        }
                    }
                }
                assert(pfind==1)
            }
            
        }

        for(i<-0 to this.totalInstNum-1){
            
            for(j<- 0 to this.totalInstNum-1){
                if(this.finalConnection(i)(j)==1){
                    if(i>=this.fanoutMappingTags.length&&j>=this.fanoutMappingTags.length){
                        var haspred=this.finalInstArray(i)(3)
                        var hasdata=this.finalInstArray(i)(2)
                        var hasinpred=this.finalInstArray(j)(0)
                        var hasindata=this.finalInstArray(j)(4)
                        assert(haspred==hasinpred)
                        assert(hasindata==hasdata)
                    }
                    else if(j>=this.fanoutMappingTags.length){
                        var haspred=this.finalInstArray(i)(3)
                        var hasdata=this.finalInstArray(i)(2)
                        var hasinpred=this.finalInstArray(j)(0)
                        var hasindata=this.finalInstArray(j)(4)
                        assert(haspred==hasinpred)
                        assert(hasindata==hasdata)
                        // println(s"inst $i has predout $haspred has dataout $hasdata and $j has predin $hasinpred has datain $hasindata")
                    
                    }
                    else if(i>=this.fanoutMappingTags.length){
                        var haspred=this.finalInstArray(i)(3)
                        var hasdata=this.finalInstArray(i)(2)
                        //find i's src
                        var pf=0
                        var pk=0
                        for(k<-0 to this.fanoutMappingTags.length-1){
                            if(this.finalConnectionbk(k)(i)==1){
                                pf=1
                                pk=k
                            
                            }
                        }
                        assert(pf==1)
                        var iisr=0
                        var tk=0
                        for(k<-0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(pk)){
                                iisr=1
                                tk=k
                            }
                        }
                        var originalNode=pk
                        if(iisr==1){
                            originalNode=tk
                        }
                        else{
                            //is routing
                            var of=0
                            while(of==0){
                                pk=pk-1
                                for(k<-0 to this.originalMappingTags.length-1){
                                    if(this.originalMappingTags(k)==this.fanoutMappingTags(pk)){
                                        of=1
                                        tk=k
                                    }
                                }
                            }
                            assert(pk>=0)
                            originalNode=tk
                        }

                        var jisr=0
                        for(k<-0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(j)){
                                jisr=1
                            }
                        }
                        var connectedNode=0
                        if(jisr==1){
                            connectedNode=this.fanoutMappingTags(j)
                        }
                        else{
                            //is routing
                            var of=0
                            pk=j
                            while(of==0){
                                pk=pk-1
                                for(k<-0 to this.originalMappingTags.length-1){
                                    if(this.originalMappingTags(k)==this.fanoutMappingTags(pk)){
                                        of=1
                                        tk=k
                                    }
                                }
                            }
                            assert(pk>=0)
                            connectedNode=this.fanoutMappingTags(pk)
                        }

                        
                        //src is get, check the dest's input
                        //now src and dest are both normal insts
                        
                        var stillKeepData0=0
                        var stillKeepData1=0
                        var stillKeepPred0=0
                        var stillKeepPred1=this.originalMappingTags(originalNode)
                        //considering src and dest may exist several edges, it should all be found out
                        // println(s"original instnum is $correspondingInst while instnum is $j")
                        // println(s"original ndoe is $i and $j at $stillKeepPred1 while end is $connectedNode")
                        var myfindd=0
                        var myfindp=0
                        
                        for(k<-0 to this.connectDataNum(originalNode)-1){
                            if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))){
                                // stillKeepData=1
                                myfindd=1
                            }
                            
                        }
                        for(k<-0 to this.connectPredNum(originalNode)-1){
                            if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))){
                                // stillKeepPred=1
                                myfindp=1
                            }
                            
                        }
                        // println(s"hasdata is $hasdata, myfindd is $myfindd")
                        // assert((hasdata==1&&myfindd>0||hasdata==0||stillKeepPred1==connectedNode))
                        // assert((haspred==1&&myfindp>0||haspred==0||stillKeepPred1==connectedNode))
                    }
                    else{
                        var haspred=this.finalInstArray(i)(3)
                        var hasdata=this.finalInstArray(i)(2)
                        //find i's src
                        var pf=0
                        var pk=i
                        // for(k<-0 to this.fanoutMappingTags.length-1){
                        //     if(this.finalConnectionbk(k)(i)==1){
                        //         pf=1
                        //         pk=k
                            
                        //     }
                        // }
                        // assert(pf==1)
                        var iisr=0
                        var tk=0
                        for(k<-0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(pk)){
                                iisr=1
                                tk=k
                            }
                        }
                        var originalNode=pk
                        if(iisr==1){
                            originalNode=tk
                        }
                        else{
                            //is routing
                            var of=0
                            while(of==0){
                                pk=pk-1
                                for(k<-0 to this.originalMappingTags.length-1){
                                    if(this.originalMappingTags(k)==this.fanoutMappingTags(pk)){
                                        of=1
                                        tk=k
                                    }
                                }
                            }
                            assert(pk>=0)
                            originalNode=tk
                        }

                        var jisr=0
                        for(k<-0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(j)){
                                jisr=1
                            }
                        }
                        var connectedNode=0
                        if(jisr==1){
                            connectedNode=this.fanoutMappingTags(j)
                        }
                        else{
                            //is routing
                            var of=0
                            pk=j
                            while(of==0){
                                pk=pk-1
                                for(k<-0 to this.originalMappingTags.length-1){
                                    if(this.originalMappingTags(k)==this.fanoutMappingTags(pk)){
                                        of=1
                                        tk=k
                                    }
                                }
                            }
                            assert(pk>=0)
                            connectedNode=this.fanoutMappingTags(pk)
                        }

                        
                        //src is get, check the dest's input
                        //now src and dest are both normal insts
                        
                        var stillKeepData0=0
                        var stillKeepData1=0
                        var stillKeepPred0=0
                        var stillKeepPred1=this.originalMappingTags(originalNode)
                        //considering src and dest may exist several edges, it should all be found out
                        // println(s"original instnum is $correspondingInst while instnum is $j")
                        // println(s"original ndoe is $i and $j at $stillKeepPred1 while end is $connectedNode")
                        var myfindd=0
                        var myfindp=0
                        
                        for(k<-0 to this.connectDataNum(originalNode)-1){
                            if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))){
                                // stillKeepData=1
                                myfindd=1
                            }
                            
                        }
                        for(k<-0 to this.connectPredNum(originalNode)-1){
                            if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))){
                                // stillKeepPred=1
                                myfindp=1
                            }
                            
                        }
                        // println(s"hasdata is $hasdata, myfindd is $myfindd")
                        // println(s"haspred is $haspred, myfindp is $myfindp")
                        // assert((myfindd==0&&hasdata==0||stillKeepPred1==connectedNode||has))
                        // assert((myfindp==0&&haspred==0||stillKeepPred1==connectedNode))
                        // assert(hasdata==1&&myfindd>0||haspred==1&&myfindp>0||myfindp==0&&myfindd==0&&hasdata==0&&haspred==0||stillKeepPred1==connectedNode)
                    }
                }

            }
        }

        return 0
    }

    def Backend2Remulation():Int={
        
        //Init all the arrays
        for(i<-0 to this.maxInst-1){
            
            this.remudataNum(i)=Array.fill(4)(0)
            // this.remuTagresolve(i)=Array.fill(perInst)(0)
            this.remuFromReg(i)=0
            this.whichReg(i)=100
            for(j<-0 to this.dataWidth*2-1){
                this.finalInstForRemu(i)(j)=0
            }
            for(j<-0 to log2Ceil(this.dataWidth)){
                this.finalInstForRemu(i)(j+9+1+this.tagWidth)=1//set masks to all-1
            }
            for(j<-0 to this.dataWidth-1){
                this.regconfig(i)(j)=0
            }
            this.regconfigTag(i)=0
        }
        for(i<-0 to this.maxInst-1){
            this.remuTagresolve0(i)=Array.fill(3)(0)
            this.remuTagresolve1(i)=Array.fill(3)(0)
            this.remuTagresolveNum(i)=0
        }
        var regNum=0
        //scan each Backend instruction and transform them
        for(i<-0 to this.totalInstNum-1){
            //check the instruction's type
            //Instruction type
            if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){//add
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1//isntype=5, add
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){//sub
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1//isntype=6, sub
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                //eq instype=7
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                //lt 8
                
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                //gt 9
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //uneq 10
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //leq 11
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //geq 12
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //sleq 15
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //sgeq 17
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //and 18
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                //or set
                if(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1){
                    if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//pred reverse or 43
                        this.finalInstForRemu(i)(this.tagWidth+0)=1
                        this.finalInstForRemu(i)(this.tagWidth+1)=1
                        this.finalInstForRemu(i)(this.tagWidth+3)=1
                        this.finalInstForRemu(i)(this.tagWidth+5)=1
                    }
                    else{//pred or 42
                        this.finalInstForRemu(i)(this.tagWidth+1)=1
                        this.finalInstForRemu(i)(this.tagWidth+3)=1
                        this.finalInstForRemu(i)(this.tagWidth+5)=1
                    }
                }
                else{//or 19
                    this.finalInstForRemu(i)(this.tagWidth+0)=1
                    this.finalInstForRemu(i)(this.tagWidth+1)=1
                    this.finalInstForRemu(i)(this.tagWidth+4)=1
                }
                
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    //get the predicate
                    if(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1){
                        this.remudataNum(i)(1)=1//d0,d1,d2
                    }
                    else{
                        this.remudataNum(i)(0)=1
                    }
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        if(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1){
                            this.remudataNum(i)(2)=1
                            this.remudataNum(i)(1)=1
                            this.remudataNum(i)(0)=1//d0,d2
                        }
                        else{
                            this.remudataNum(i)(0)=0//d0
                        }
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        if(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1){
                            this.remudataNum(i)(3)=1//d1 and d2
                        }
                        else{
                            this.remudataNum(i)(2)=1
                            this.remudataNum(i)(1)=1//d1
                        }
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                // /bshl 21
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                // /bshr 22
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                // /bshrl 23
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){

                this.finalInstForRemu(i)(this.tagWidth+3)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //xor 24
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+5)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //mul 39
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //udiv 25
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //sdiv 26
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==0){
                
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                this.finalInstForRemu(i)(this.tagWidth+5)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //urem 60
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                this.finalInstForRemu(i)(this.tagWidth+5)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //srem 61
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                if(this.finalInstArray(i)(0)==0&&this.finalInstArray(i)(1)==0){//normal loopoutR is translated when seeing a non-pred imm gen
                    this.finalInstForRemu(i)(this.tagWidth+0)=1
                    this.finalInstForRemu(i)(this.tagWidth+1)=0
                    this.finalInstForRemu(i)(this.tagWidth+4)=0
                    //set the which is imm
                    this.finalInstForRemu(i)(this.tagWidth+8)=1

                    
                    // this.remudataNum(i)(0)=0//d0
                    // this.remuFromReg(i)=1//d0 come from regfile
                    this.remudataNum(i)(2)=1
                    this.remudataNum(i)(1)=1//only d1
                    this.remuFromReg(i)=2//d1 come from regfile
                    this.whichReg(i)=this.remuTagresolveNum(i)
                    this.regconfigTag(i)=this.totalInstNum+1+regNum
                    this.remuTagresolve0(i)(this.remuTagresolveNum(i))=this.totalInstNum+1+regNum
                    this.remuTagresolve1(i)(this.remuTagresolveNum(i))=2//this is for d1
                    this.remuTagresolveNum(i)=this.remuTagresolveNum(i)+1
                    regNum=regNum+1
                    for(j<-0 to this.dataWidth-1){
                        
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                    this.regconfig(i)(0)=1
                }
                else if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==0){//loopoutR 0, need to be distinguished from the philoop predicated one
                    //scan the connection
                    var setPhi=0
                    for(k<-0 to this.totalInstNum-1){
                        if(/*this.finalConnection(k)(i)==1*/this.oriConnection(k)(i)==1){
                            if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                
                                setPhi=1
                            }
                            if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                setPhi=1
                            }
                            if(this.finalInstArray(k)(7)==1&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==0&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                setPhi=1
                            }
                            if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                setPhi=1
                            }
                            if(this.finalInstArray(k)(7)==1&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==0&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                setPhi=1
                            }
                        }
                    }
                    if(setPhi==0){//is a predicated loopoutR
                        this.finalInstForRemu(i)(this.tagWidth+0)=1
                        this.finalInstForRemu(i)(this.tagWidth+1)=0
                        this.finalInstForRemu(i)(this.tagWidth+3)=0
                        this.finalInstForRemu(i)(this.tagWidth+5)=0
                        //set the which is imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        for(j<-0 to this.dataWidth-1){
                            this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        }

                        this.remudataNum(i)(2)=1
                        
                        this.remudataNum(i)(1)=1//only d1
                        
                    }
                    else{//set data triggered 44
                        this.finalInstForRemu(i)(this.tagWidth+2)=1
                        this.finalInstForRemu(i)(this.tagWidth+3)=1
                        this.finalInstForRemu(i)(this.tagWidth+5)=1

                        for(j<-0 to this.dataWidth-1){
                            this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        }
                        this.remudataNum(i)(0)=0//d0
                    }
                }
                else if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//pred loopout 1

                    this.finalInstForRemu(i)(this.tagWidth+0)=0
                    this.finalInstForRemu(i)(this.tagWidth+1)=0
                    this.finalInstForRemu(i)(this.tagWidth+3)=0
                    this.finalInstForRemu(i)(this.tagWidth+5)=0
                    //set the which is imm
                    
                    this.finalInstForRemu(i)(this.tagWidth+8)=1
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }

                    this.remudataNum(i)(2)=1
                    
                    this.remudataNum(i)(1)=1//only d1
                }
                else{
                    assert(1==0)
                }
                //gen, for gen two inst should be generated, if it has no predicate then it will be a or otherwise it will be a predicated or
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                //loopout data 0 or 1
                if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//reverse loopout
                    this.finalInstForRemu(i)(this.tagWidth+0)=1
                }
                else{
                    this.finalInstForRemu(i)(this.tagWidth+0)=0
                }
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){//only imm
                    this.remudataNum(i)(2)=1
                    this.remudataNum(i)(1)=1//d1
                    this.finalInstForRemu(i)(this.tagWidth+8)=1
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                else{
                    this.remudataNum(i)(0)=1//d0,d1
                }
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                //loopout pred 0 or 1
                if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//reverse loopout, never access this branch
                    this.finalInstForRemu(i)(this.tagWidth+0)=1
                    assert(1==0)
                }
                else{
                    this.finalInstForRemu(i)(this.tagWidth+0)=0
                }

                assert(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1)
                this.remudataNum(i)(0)=1//d0,d1
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //slt 13
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                
                // finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //sgt 14
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                this.finalInstForRemu(i)(this.tagWidth+5)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //fmul 62
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+3)=1
                this.finalInstForRemu(i)(this.tagWidth+4)=1
                this.finalInstForRemu(i)(this.tagWidth+5)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //fadd 63
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+6)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //fdiv 64
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+6)=1
                if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//two input
                    this.remudataNum(i)(0)=1
                }
                else{
                    if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 input with imm
                        this.finalInstForRemu(i)(this.tagWidth+9)=1
                        this.remudataNum(i)(0)=0
                    }
                    else{//101 d1 input with imm, d0 comes from imm
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1
                    }
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                }
                //fsub 65
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                if(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1){//pred fp
                    
                    var setPhi=0
                    for(k<-0 to this.totalInstNum-1){
                        if(this.finalConnection(k)(i)==1){
                            if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                
                                setPhi=1
                            }
                            if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                setPhi=1
                            }
                        }
                    }
                    if(setPhi==0){//is a predicated fp, d0/d1
                        
                        //set the which is imm
                        if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            this.remudataNum(i)(2)=1
                            
                            this.remudataNum(i)(1)=1//only d1
                            if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//72
                                this.finalInstForRemu(i)(this.tagWidth+3)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                            else{//68
                                this.finalInstForRemu(i)(this.tagWidth+2)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                        }
                        else{
                            this.remudataNum(i)(0)=1//d0 and d1
                            if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//72
                                this.finalInstForRemu(i)(this.tagWidth+3)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                            else{//68
                                this.finalInstForRemu(i)(this.tagWidth+2)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                        }

                        
                    }
                    else{//set data triggered 70
                        this.finalInstForRemu(i)(this.tagWidth+2)=1
                        this.finalInstForRemu(i)(this.tagWidth+1)=1
                        this.finalInstForRemu(i)(this.tagWidth+6)=1

                        if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            this.remudataNum(i)(2)=1
                            
                            this.remudataNum(i)(1)=1//only d1
                        }
                        else{
                            this.remudataNum(i)(0)=1//d0 and d1
                        }
                    }
                }
                else{
                    this.finalInstForRemu(i)(this.tagWidth+1)=1
                    this.finalInstForRemu(i)(this.tagWidth+6)=1
                    if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){//loopout 
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.finalInstForRemu(i)(this.tagWidth+1)=0
                        this.finalInstForRemu(i)(this.tagWidth+6)=0
                        for(j<-0 to this.dataWidth-1){
                            this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        }
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1//only d1
                        this.remuFromReg(i)=2
                        this.whichReg(i)=this.remuTagresolveNum(i)
                        this.regconfigTag(i)=this.totalInstNum+1+regNum
                        this.remuTagresolve0(i)(this.remuTagresolveNum(i))=this.totalInstNum+1+regNum
                        this.remuTagresolve1(i)(this.remuTagresolveNum(i))=2//this is for d1
                        this.remuTagresolveNum(i)=this.remuTagresolveNum(i)+1
                        regNum=regNum+1
                        this.regconfig(i)(0)=1
                        // for(j<-0 to this.dataWidth-1){
                        
                        //     this.regconfig(i)(j)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        // }
                    }
                    else{//66
                        this.remudataNum(i)(0)=0//d0 only
                    }
                }
                //fp2int 66 and pred 68, data triggered 70, pred reversed 72
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                // if(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1){
                //     this.finalInstForRemu(i)(this.tagWidth+0)=1
                //     this.finalInstForRemu(i)(this.tagWidth+2)=1
                //     this.finalInstForRemu(i)(this.tagWidth+6)=1
                // }
                // else{
                //     this.finalInstForRemu(i)(this.tagWidth+0)=1
                //     this.finalInstForRemu(i)(this.tagWidth+1)=1
                //     this.finalInstForRemu(i)(this.tagWidth+6)=1
                // }

                if(this.finalInstArray(i)(0)==1||this.finalInstArray(i)(1)==1){//pred fp
                    
                    var setPhi=0
                    for(k<-0 to this.totalInstNum-1){
                        if(/*this.finalConnection(k)(i)==1*/this.oriConnection(k)(i)==1){
                            if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                
                                setPhi=1
                            }
                            if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                setPhi=1
                            }
                        }
                    }
                    if(setPhi==0){//is a predicated fp, d0/d1
                        
                        //set the which is imm
                        if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            this.remudataNum(i)(2)=1
                            
                            this.remudataNum(i)(1)=1//only d1
                            if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//73
                                this.finalInstForRemu(i)(this.tagWidth+0)=1
                                
                                this.finalInstForRemu(i)(this.tagWidth+3)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                            else{//69
                                this.finalInstForRemu(i)(this.tagWidth+0)=1
                                this.finalInstForRemu(i)(this.tagWidth+2)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                        }
                        else{
                            this.remudataNum(i)(0)=1//d0 and d1
                            if(this.finalInstArray(i)(0)==1&&this.finalInstArray(i)(1)==1){//73
                                this.finalInstForRemu(i)(this.tagWidth+0)=1
                                
                                this.finalInstForRemu(i)(this.tagWidth+3)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                            else{//69
                                this.finalInstForRemu(i)(this.tagWidth+0)=1
                                this.finalInstForRemu(i)(this.tagWidth+2)=1
                                this.finalInstForRemu(i)(this.tagWidth+6)=1
                            }
                        }

                        
                    }
                    else{//set data triggered 71
                        this.finalInstForRemu(i)(this.tagWidth+2)=1
                        this.finalInstForRemu(i)(this.tagWidth+1)=1
                        this.finalInstForRemu(i)(this.tagWidth+0)=1
                        this.finalInstForRemu(i)(this.tagWidth+6)=1

                        if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            this.remudataNum(i)(2)=1
                            
                            this.remudataNum(i)(1)=1//only d1
                        }
                        else{
                            this.remudataNum(i)(0)=1//d0 and d1
                        }
                    }
                }
                else{//67
                    this.finalInstForRemu(i)(this.tagWidth+0)=1
                    this.finalInstForRemu(i)(this.tagWidth+1)=1
                    this.finalInstForRemu(i)(this.tagWidth+6)=1
                    if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        this.finalInstForRemu(i)(this.tagWidth+0)=0
                        this.finalInstForRemu(i)(this.tagWidth+1)=0
                        this.finalInstForRemu(i)(this.tagWidth+6)=0
                        for(j<-0 to this.dataWidth-1){
                            this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        }
                        this.remudataNum(i)(2)=1
                        this.remudataNum(i)(1)=1//only d0
                        this.remuFromReg(i)=2
                        this.whichReg(i)=this.remuTagresolveNum(i)
                        this.regconfigTag(i)=this.totalInstNum+1+regNum
                        this.remuTagresolve0(i)(this.remuTagresolveNum(i))=this.totalInstNum+1+regNum
                        this.remuTagresolve1(i)(this.remuTagresolveNum(i))=2//this is for d1
                        this.remuTagresolveNum(i)=this.remuTagresolveNum(i)+1
                        regNum=regNum+1
                        // for(j<-0 to this.dataWidth-1){
                        
                        //     this.regconfig(i)(j)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        // }
                        this.regconfig(i)(0)=1
                    }
                    else{
                        this.remudataNum(i)(0)=0//d0 only
                    }
                }
                //int2fp 67 and pred 69, data triggered 71, pred reversed 73
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                
                if(this.finalInstArray(i)(0)==0&&this.finalInstArray(i)(1)==0){//no pred load
                    if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){//imm load
                        this.finalInstForRemu(i)(this.tagWidth+0)=1
                        this.finalInstForRemu(i)(this.tagWidth+1)=1
                        this.finalInstForRemu(i)(this.tagWidth+2)=1
                        this.finalInstForRemu(i)(this.tagWidth+3)=1
                        this.finalInstForRemu(i)(this.tagWidth+4)=1
                        //pred comes from regfile
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        for(j<-0 to this.dataWidth-1){
                            this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        }
                        this.remudataNum(i)(2)=1
                        
                        this.remudataNum(i)(1)=1//only d1
                        this.remuFromReg(i)=2
                        this.whichReg(i)=this.remuTagresolveNum(i)
                        this.regconfigTag(i)=this.totalInstNum+1+regNum
                        this.remuTagresolve0(i)(this.remuTagresolveNum(i))=this.totalInstNum+1+regNum
                        this.remuTagresolve1(i)(this.remuTagresolveNum(i))=2//this is for d1
                        this.remuTagresolveNum(i)=this.remuTagresolveNum(i)+1
                        regNum=regNum+1
                        this.regconfig(i)(0)=1

                    }
                    else{
                        this.finalInstForRemu(i)(this.tagWidth+0)=1
                        this.finalInstForRemu(i)(this.tagWidth+2)=1
                        this.finalInstForRemu(i)(this.tagWidth+3)=1
                        this.finalInstForRemu(i)(this.tagWidth+4)=1
                        this.remudataNum(i)(0)=0//d0 only
                    }
                }
                else{
                    this.finalInstForRemu(i)(this.tagWidth+0)=1
                    this.finalInstForRemu(i)(this.tagWidth+1)=1
                    this.finalInstForRemu(i)(this.tagWidth+2)=1
                    this.finalInstForRemu(i)(this.tagWidth+3)=1
                    this.finalInstForRemu(i)(this.tagWidth+4)=1
                    if(this.finalInstArray(i)(4)==0&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==1){//imm load
                        this.finalInstForRemu(i)(this.tagWidth+8)=1
                        for(j<-0 to this.dataWidth-1){
                            this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                        }
                        this.remudataNum(i)(2)=1
                        
                        this.remudataNum(i)(1)=1//only d1
                    }
                    else{
                        this.remudataNum(i)(0)=1//d0 d1
                    }
                }
                //load 29 or load pred 31
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                if(this.finalInstArray(i)(0)==0&&this.finalInstArray(i)(1)==0){//no pred stroe
                    if(this.finalInstArray(i)(3)==0){//no out
                        this.finalInstForRemu(i)(this.tagWidth+1)=1
                        this.finalInstForRemu(i)(this.tagWidth+2)=1
                        this.finalInstForRemu(i)(this.tagWidth+3)=1
                        this.finalInstForRemu(i)(this.tagWidth+4)=1
                        if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 and imm
                            this.finalInstForRemu(i)(this.tagWidth+9)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            
                            this.remudataNum(i)(0)=0//only d0
                        }
                        else if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==0&&this.finalInstArray(i)(6)==1){//d1 and imm
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            
                            this.remudataNum(i)(2)=1
                        
                            this.remudataNum(i)(1)=1//only d1
                        }
                        else{
                            this.remudataNum(i)(0)=1//both d1 and d2
                        }
                    }
                    else{
                        this.finalInstForRemu(i)(this.tagWidth+4)=1
                        if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 and imm
                            this.finalInstForRemu(i)(this.tagWidth+9)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            
                            this.remudataNum(i)(0)=0//only d0
                        }
                        else if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==0&&this.finalInstArray(i)(6)==1){//d1 and imm
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            
                            this.remudataNum(i)(2)=1
                        
                            this.remudataNum(i)(1)=1//only d1
                        }
                        else{
                            this.remudataNum(i)(0)=1
                        }
                    }
                }
                else{
                    if(this.finalInstArray(i)(3)==1){//has out
                        this.finalInstForRemu(i)(this.tagWidth+0)=1
                        this.finalInstForRemu(i)(this.tagWidth+5)=1
                        if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 and imm
                            this.finalInstForRemu(i)(this.tagWidth+9)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            this.remudataNum(i)(2)=1
                            this.remudataNum(i)(1)=1
                            this.remudataNum(i)(0)=1//only d0, d2
                        }
                        else if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==0&&this.finalInstArray(i)(6)==1){//d1 and imm
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                        
                            this.remudataNum(i)(3)=1//only d1 d2
                        }
                        else{
                            this.remudataNum(i)(1)=1//d0 d1 d2
                        }
                    }
                    else{
                        this.finalInstForRemu(i)(this.tagWidth+5)=1
                        if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 and imm
                            this.finalInstForRemu(i)(this.tagWidth+9)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                            this.remudataNum(i)(2)=1
                            this.remudataNum(i)(1)=1
                            this.remudataNum(i)(0)=1//only d0, d2
                        }
                        else if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==0&&this.finalInstArray(i)(6)==1){//d1 and imm
                            this.finalInstForRemu(i)(this.tagWidth+8)=1
                            for(j<-0 to this.dataWidth-1){
                                this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                            }
                        
                            this.remudataNum(i)(3)=1//only d1 d2
                        }
                        else{
                            this.remudataNum(i)(1)=1//d0 d1 d2
                        }
                    }
                }
                //store 30, pred store no out 32 , pred store out  33,store out 16
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==1&&this.finalInstArray(i)(6)==0){//d0 and imm
                    this.finalInstForRemu(i)(this.tagWidth+9)=1
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                    // this.remudataNum(i)(2)=1
                    // this.remudataNum(i)(1)=1
                    // this.remudataNum(i)(0)=1//only d0, d2
                    this.remudataNum(i)(3)=1
                    this.remudataNum(i)(1)=1//only d2
                }
                else if(this.finalInstArray(i)(4)==1&&this.finalInstArray(i)(5)==0&&this.finalInstArray(i)(6)==1){//d1 and imm
                    this.finalInstForRemu(i)(this.tagWidth+8)=1
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                
                    // this.remudataNum(i)(3)=1//only d1 d2
                    this.remudataNum(i)(3)=1
                    this.remudataNum(i)(1)=1//only d2
                }
                else{
                    // this.remudataNum(i)(1)=1//d0 d1 d2
                    this.remudataNum(i)(3)=1
                    this.remudataNum(i)(1)=1//only d2
                }
                //phi 3
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==1&&this.finalInstArray(i)(12)==0){
                
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                //three input never comes from imm
                this.remudataNum(i)(3)=1//d0 d1 d2
                this.remudataNum(i)(0)=1
                if(this.finalInstArray(i)(13)==0&&this.finalInstArray(i)(14)==0&&this.finalInstArray(i)(15)==1){
                    this.remuFromReg(i)=1
                    this.whichReg(i)=this.remuTagresolveNum(i)
                    this.regconfigTag(i)=this.totalInstNum+1+regNum
                    this.remuTagresolve0(i)(this.remuTagresolveNum(i))=this.totalInstNum+1+regNum
                    this.remuTagresolve1(i)(this.remuTagresolveNum(i))=1//this is for d0
                    this.remuTagresolveNum(i)=this.remuTagresolveNum(i)+1
                    regNum=regNum+1
                    for(j<-0 to this.dataWidth-1){
                        
                        this.regconfig(i)(j)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                    assert(1==0)
                }
                //philoop 2
            }
            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                this.finalInstForRemu(i)(this.tagWidth+1)=1

                if(this.finalInstArray(i)(13)==1&&this.finalInstArray(i)(14)==0&&this.finalInstArray(i)(15)==0){//d1 and imm, d2
                    this.finalInstForRemu(i)(this.tagWidth+8)=1
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                    // this.remudataNum(i)(3)=1
                    //only d1, d2
                    this.remudataNum(i)(3)=1
                    this.remudataNum(i)(1)=1//only d2
                }
                else if(this.finalInstArray(i)(13)==0&&this.finalInstArray(i)(14)==1&&this.finalInstArray(i)(15)==0){//d0 and imm d2
                    this.finalInstForRemu(i)(this.tagWidth+9)=1
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                    // this.remudataNum(i)(0)=1
                    // this.remudataNum(i)(1)=1
                    // this.remudataNum(i)(2)=1//only d0 d2
                    this.remudataNum(i)(3)=1
                    this.remudataNum(i)(1)=1//only d2
                }
                else{
                    // this.remudataNum(i)(1)=1//d0 d1 d2
                    this.remudataNum(i)(3)=1
                    this.remudataNum(i)(1)=1//only d2
                }
                //phi control 3
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==0&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                
                this.finalInstForRemu(i)(this.tagWidth+1)=1
                //philoop control 2

                if(this.finalInstArray(i)(13)==1&&this.finalInstArray(i)(14)==1&&this.finalInstArray(i)(15)==0){//d1 and imm, d2
                    this.finalInstForRemu(i)(this.tagWidth+8)=0
                    for(j<-0 to this.dataWidth-1){
                        this.finalInstForRemu(i)(j+9+1+log2Ceil(this.dataWidth)+1+this.tagWidth)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)
                    }
                    this.remudataNum(i)(3)=1//d0 d1, d2
                    this.remudataNum(i)(0)=1
                    this.remuFromReg(i)=1
                    this.whichReg(i)=this.remuTagresolveNum(i)
                    this.regconfigTag(i)=this.totalInstNum+1+regNum
                    this.remuTagresolve0(i)(this.remuTagresolveNum(i))=this.totalInstNum+1+regNum
                    this.remuTagresolve1(i)(this.remuTagresolveNum(i))=1//this is for d0
                    this.remuTagresolveNum(i)=this.remuTagresolveNum(i)+1
                    regNum=regNum+1
                    for(j<-0 to this.dataWidth-1){
                        
                        this.regconfig(i)(j)=this.finalInstArray(i)(2*this.dataWidth-this.dataWidth+j)//normally it is 1
                    }
                    
                }
                
                else{
                    this.remudataNum(i)(3)=1//d0 d1, d2
                    this.remudataNum(i)(0)=1
                }
            }

            else if(this.finalInstArray(i)(7)==1&&this.finalInstArray(i)(8)==1&&this.finalInstArray(i)(9)==0&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                //routing translated into add 0
                this.finalInstForRemu(i)(this.tagWidth+9)=1
                this.remudataNum(i)(0)=0//d0
                
            }
            else if(this.finalInstArray(i)(7)==0&&this.finalInstArray(i)(8)==0&&this.finalInstArray(i)(9)==1&&this.finalInstArray(i)(10)==1&&this.finalInstArray(i)(11)==0&&this.finalInstArray(i)(12)==1){
                //mergepred is translated into add
                this.finalInstForRemu(i)(this.tagWidth+2)=1
                this.finalInstForRemu(i)(this.tagWidth+0)=1
                
                this.finalInstForRemu(i)(this.tagWidth+9)=0
                // this.remudataNum(i)(0)=0//d0
                this.remudataNum(i)(0)=1//d0 d1
                println("got mergepred")
            }
            else{
                println("ERROR AT INSTTYPE")
            }
            //check if the instruction has any pred out or data out
        }
        return 0
    }

    def analyzeRemulationPass0(): Int = {
        //set the output configuration here
        //this is the final processing for  architectures
        //for each edge, find the src pe and dest pe, deceide the connection is pred/data, then set the outarray
        // this.CGRAGen()
        for(i<-0 to this.maxInst-1){
            
            this.remuoutSetD0(i)=Array.fill(this.outSize)(0)
            this.remuoutSetD1(i)=Array.fill(this.outSize)(0)
            this.remuoutSetD2(i)=Array.fill(this.outSize)(0)
            

        }
        
        for(i<-0 to this.totalInstNum-1){
            for(j<- 0 to this.totalInstNum-1){
                if(this.finalConnection(i)(j)==1){
                    var hasSet=0
                    var destPE=this.finalplacements(j)//get dest PE
                    var srcPE=this.finalplacements(i)//get src PE
                    if(i==42&&j==43){
                        println(s"destPE $j and $destPE, srcPE $i and $srcPE")
                    }
                    //check which out bit is the dest
                    var numberPE=1
                    var find=0
                    if(destPE==srcPE){
                        numberPE=0
                    }
                    else{
                        var thisf=0
                        for(k<-0 to this.CGRASize-1){
                            if(this.CGRA_array(srcPE)(k)==1&&k!=srcPE){
                                
                                if(k==destPE){
                                    find=1
                                    thisf=1
                                }
                                if(find==0){
                                    numberPE=numberPE+1
                                }
                            }
                        }
                        assert(thisf==1)
                    }
                    var fanoutRouting=1
                    if(j<this.fanoutMappingTags.length){//j belongs the original routed PE
                        for(k<- 0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(j)){
                                fanoutRouting=0
                            }
                        }
                    }
                    //numberPE is the outbit number
                    if(j>=this.fanoutMappingTags.length||fanoutRouting==1){//for routing destination, simply connect the data0 and pred0
                        if(this.finalInstArray(i)(2)==1){
                            
                            hasSet=1
                            this.remuoutSetD0(i)(numberPE)=1
                            
                            this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                            this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1//this is for d0
                            this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                        }
                        else if(this.finalInstArray(i)(3)==1){
                            
                            hasSet=1
                            this.remuoutSetD0(i)(numberPE)=1
                            this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                            this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1//this is for d0
                            this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                        }
                        else{
                            assert(1==0)
                        }
                    }
                    
                    else{//need to traverse the connection to decide which input port should be connected
                        var isrouting=1
                        var correspondingInst=i
                        if(i<this.fanoutMappingTags.length){
                            //check whether the src is a routing node, if it is a routing node, try to find the original node
                            for(k<- 0 to this.originalMappingTags.length-1){
                                if(this.originalMappingTags(k)==this.fanoutMappingTags(i)){
                                    isrouting=0
                                }
                            }
                        }
                        var originalNode=correspondingInst
                        if(isrouting==0){
                            originalNode=correspondingInst
                        }
                        else{//source back to the original
                            if(i<this.fanoutMappingTags.length){
                                var findo=0
                                while(originalNode-1>=0&&findo==0){
                                    originalNode=originalNode-1
                                    for(k<- 0 to this.originalMappingTags.length-1){
                                        if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                            findo=1
                                        }
                                    }
                                    
                                }
                                assert(originalNode>=0&&findo==1)
                            }
                            else{
                                var myInst=0
                                
                                for(m<- 0 to this.fanoutMappingTags.length-1){
                                    if(this.finalConnectionbk(m)(i)==1){
                                        myInst=m//find src inst
                                    }
                                }
                                var isroutingbk=1
                                //check if it is another routing
                                for(k<- 0 to this.originalMappingTags.length-1){
                                    if(this.originalMappingTags(k)==this.fanoutMappingTags(myInst)){
                                        isroutingbk=0
                                    }
                                }
                                originalNode=myInst
                                
                                if(isroutingbk==1){
                                    // println("Routing for routing")
                                    var findo=0
                                    while(originalNode-1>=0&&findo==0){
                                        originalNode=originalNode-1
                                        for(k<- 0 to this.originalMappingTags.length-1){
                                            if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                                findo=1
                                            }
                                        }
                                        
                                    }
                                    assert(originalNode>=0&&findo==1)
                                }
                                // println(s"Routing $originalNode")
                            }
                        }
                        var realOriginal=0
                        var rf=0
                        for(k<- 0 to this.originalMappingTags.length-1){
                            if(this.originalMappingTags(k)==this.fanoutMappingTags(originalNode)){
                                realOriginal=k
                                rf=1
                            }
                        }
                        assert(rf==1)
                        originalNode=realOriginal
                        //src is get, check the dest's input
                        //now src and dest are both normal insts
                        var connectedNode=this.fanoutMappingTags(j)
                        var stillKeepData0=0
                        var stillKeepData1=0
                        var stillKeepPred0=0
                        var stillKeepPred1=0
                        //considering src and dest may exist several edges, it should all be found out
                        // println(s"original instnum is $correspondingInst while instnum is $j")
                        // println(s"original ndoe is $originalNode while end is $connectedNode")
                        var myfindd=0
                        var myfindp=0
                        for(k<-0 to this.connectDataNum(originalNode)-1){
                            if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))&&myfindd==0){
                                stillKeepData0=this.OutDataPENum(originalNode)(k)
                                myfindd=1
                            }
                            else if(this.OutDataPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutDataPE(originalNode)(k))&&myfindd==1){
                                stillKeepData1=this.OutDataPENum(originalNode)(k)
                                myfindd=2
                            }
                        }
                        for(k<-0 to this.connectPredNum(originalNode)-1){
                            if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))&&myfindp==0){
                                stillKeepPred0=this.OutPredPENum(originalNode)(k)
                                myfindp=1
                            }
                            else if(this.OutPredPENum(originalNode)(k)!=0&&connectedNode==this.originalMappingTags(this.OutPredPE(originalNode)(k))&&myfindp==1){
                                stillKeepPred1=this.OutPredPENum(originalNode)(k)
                                myfindp=2
                            }
                        }
                        assert(myfindd>0||myfindp>0)
                        if(i==42&&j==43){
                            println(s"connctions data $stillKeepData0 and $stillKeepData1, pred $stillKeepPred0 and $stillKeepPred1")
                        }
                        if(stillKeepData0!=0){
                            if(stillKeepData0==1){
                                // this.outSetD0(i)(numberPE)=1
                                // 

                                //for data input only need to resolve the special cases for the controlphi(loop),  loopoutPred, predicated gen
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    this.remuoutSetD2(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3//this is for d2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    this.remuoutSetD2(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD1(i)(numberPE)=1
                                    //loopout pred
                                    if(i==42&&j==43){
                                        println(s"has a loopout pred at $i and $numberPE")
                                    }
                                    this.remuoutSetD1(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else{
                                    this.remuoutSetD0(i)(numberPE)=1//for data out the temporal and spatial shares the same port
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }

                            }
                            else{
                                // this.outSetD1(i)(numberPE)=1
                                // this.remuoutSetD1(i)(numberPE)=1
                                //for data input only need to resolve the special cases for the controlphi(loop),  loopoutPred, predicated gen
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    assert(1==0)
                                    this.remuoutSetD2(i)(numberPE)=1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    assert(1==0)
                                    this.remuoutSetD2(i)(numberPE)=1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    assert(1==0)
                                    this.remuoutSetD1(i)(numberPE)=1
                                    //loopout pred
                                }
                                else{
                                    this.remuoutSetD1(i)(numberPE)=1//for data out the temporal and spatial shares the same port
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                            }
                            hasSet=1
                        }
                        if(stillKeepData1!=0){
                            if(stillKeepData1==1){
                                // this.outSetD0(i)(numberPE)=1
                                // this.remuoutSetD0(i)(numberPE)=1
                                //for data input only need to resolve the special cases for the controlphi(loop),  loopoutPred, predicated gen
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    this.remuoutSetD2(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    this.remuoutSetD2(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD1(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //loopout pred
                                }
                                else{
                                    this.remuoutSetD0(i)(numberPE)=1//for data out the temporal and spatial shares the same port
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                            }
                            else{
                                // this.outSetD1(i)(numberPE)=1
                                // this.remuoutSetD1(i)(numberPE)=1
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    assert(1==0)
                                    this.remuoutSetD2(i)(numberPE)=1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    assert(1==0)
                                    this.remuoutSetD2(i)(numberPE)=1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    assert(1==0)
                                    this.remuoutSetD1(i)(numberPE)=1
                                    //loopout pred
                                }
                                else{
                                    this.remuoutSetD1(i)(numberPE)=1//for data out the temporal and spatial shares the same port
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                            }
                            hasSet=1
                        }
                        if(stillKeepPred0!=0){
                            hasSet=1
                            if(stillKeepPred0==1){
                                // this.outSetP0(i)(numberPE)=1
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    this.remuoutSetD0(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    this.remuoutSetD0(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD0(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //loopout pred
                                }
                                else if( this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //pred or
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //pred store
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    // phi
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //philoop
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    if(this.finalInstArray(j)(0)==1&&this.finalInstArray(j)(1)==0){//loopout 0, need to be distinguished from the philoop predicated one
                                        //scan the connection
                                        var setPhi=0
                                        for(k<-0 to this.totalInstNum-1){
                                            if(/*this.finalConnection(k)(j)==1*/this.oriConnection(k)(j)==1){
                                                if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                                    
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==1&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==0&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==1&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==0&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                                    setPhi=1
                                                }
                                            }
                                        }
                                        if(setPhi==1){
                                            this.remuoutSetD0(i)(numberPE)=1
                                            this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                            assert(this.finalInstForRemu(j)(this.tagWidth+0)!=1||this.finalInstForRemu(j)(this.tagWidth+1)!=0||this.finalInstForRemu(j)(this.tagWidth+3)!=0||this.finalInstForRemu(j)(this.tagWidth+5)!=0||this.finalInstForRemu(j)(this.tagWidth+8)!=1)
                                        }   
                                        else{
                                            this.remuoutSetD1(i)(numberPE)=1
                                            this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                        }
                                        this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                        this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    }
                                    else{
                                        assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                        this.remuoutSetD1(i)(numberPE)=1
                                        this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                        this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                        this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    }
                                    //pred gen
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD0(i)(numberPE)=1
                                    // assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //mergepred
                                }
                                else{
                                    this.remuoutSetD1(i)(numberPE)=1//for data out the temporal and spatial shares the same port
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                            }
                            else{
                                // this.outSetP1(i)(numberPE)=1
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    this.remuoutSetD1(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD1(i)(numberPE)=1
                                    // assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //mergepred
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    this.remuoutSetD1(i)(numberPE)=1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else{
                                    assert(1==0)
                                }
                            }
                        }
                        if(stillKeepPred1!=0){
                            hasSet=1
                            if(stillKeepPred1==1){
                                // this.outSetP0(i)(numberPE)=1
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    this.remuoutSetD0(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    this.remuoutSetD0(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD0(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //loopoutpred
                                }
                                else if( this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    //pred or
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    //pred store
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    // phi
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    this.remuoutSetD2(i)(numberPE)=1
                                    assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    //philoop
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=3
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==1&&this.finalInstArray(j)(12)==0){
                                    if(this.finalInstArray(j)(0)==1&&this.finalInstArray(j)(1)==0){//loopout 0, need to be distinguished from the philoop predicated one
                                        //scan the connection
                                        var setPhi=0
                                        for(k<-0 to this.totalInstNum-1){
                                            if(/*this.finalConnection(k)(j)==1*/this.oriConnection(k)(j)==1){
                                                if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                                    
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==1&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==0&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==0&&this.finalInstArray(k)(8)==0&&this.finalInstArray(k)(9)==1&&this.finalInstArray(k)(10)==1&&this.finalInstArray(k)(11)==1&&this.finalInstArray(k)(12)==0){
                                                    setPhi=1
                                                }
                                                if(this.finalInstArray(k)(7)==1&&this.finalInstArray(k)(8)==1&&this.finalInstArray(k)(9)==0&&this.finalInstArray(k)(10)==0&&this.finalInstArray(k)(11)==0&&this.finalInstArray(k)(12)==1){
                                                    setPhi=1
                                                }
                                            }
                                        }
                                        if(setPhi==1){
                                            this.remuoutSetD0(i)(numberPE)=1
                                            this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                            assert(this.finalInstForRemu(j)(this.tagWidth+0)!=1||this.finalInstForRemu(j)(this.tagWidth+1)!=0||this.finalInstForRemu(j)(this.tagWidth+3)!=0||this.finalInstForRemu(j)(this.tagWidth+5)!=0||this.finalInstForRemu(j)(this.tagWidth+8)!=1)
                                        
                                        }   
                                        else{
                                            this.remuoutSetD1(i)(numberPE)=1
                                            this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                        }
                                    }
                                    else{
                                        assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                        this.remuoutSetD1(i)(numberPE)=1
                                        this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    }
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //pred gen
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD0(i)(numberPE)=1
                                    // assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=1
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //mergepred
                                }
                                else{
                                    this.remuoutSetD1(i)(numberPE)=1//for data out the temporal and spatial shares the same port
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                            }
                            else{
                                // this.outSetP1(i)(numberPE)=1
                                if(this.finalInstArray(j)(7)==1&&this.finalInstArray(j)(8)==1&&this.finalInstArray(j)(9)==0&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control phi
                                    this.remuoutSetD1(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==1&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    this.remuoutSetD1(i)(numberPE)=1
                                    // assert(this.finalInstArray(j)(0)==1||this.finalInstArray(j)(1)==1)
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                    //mergepred
                                }
                                else if(this.finalInstArray(j)(7)==0&&this.finalInstArray(j)(8)==0&&this.finalInstArray(j)(9)==1&&this.finalInstArray(j)(10)==0&&this.finalInstArray(j)(11)==0&&this.finalInstArray(j)(12)==1){
                                    //control philoop
                                    this.remuoutSetD1(i)(numberPE)=1
                                    this.remuTagresolve0(j)(this.remuTagresolveNum(j))=i+1
                                    this.remuTagresolve1(j)(this.remuTagresolveNum(j))=2
                                    this.remuTagresolveNum(j)=this.remuTagresolveNum(j)+1
                                }
                                else{
                                    assert(1==0)
                                }
                            }
                        }
                        assert(hasSet==1)
                    }
                }
            }
        }
        // for(i<-0 to this.totalInstNum-1){
            
        //     println(s"remuTagresolve0: $i " )
        //     for(j<- 0 to this.remuTagresolveNum(i)-1)
        //     {
        //         print(" "+this.remuTagresolve0(i)(j))
        //     }
        //     println()
        // }
        // for(i<-0 to this.totalInstNum-1){
            
        //     println(s"remuTagresolve1: $i " )
        //     for(j<- 0 to this.remuTagresolveNum(i)-1)
        //     {
        //         print(" "+this.remuTagresolve1(i)(j))
        //     }
        //     println()
        // }
        return 0
    }

    def analyzeRemulationPass1():Int={
        // resolve the redistributed set, that is the input tag resolver
        //parse each remuTagresolve and formulate the restribute array
        
        // var remuTagresolve0:Array[Array[Int]] = Array.ofDim[Int](maxInst, 3*tagWidth)
        // var remuTagresolve1:Array[Array[Int]] = Array.ofDim[Int](maxInst, 3)
        // var remuTagresolveNum:Array[Int]=new Array[Int](maxInst)

        // var remuFromReg:Array[Int]=new Array[Int](maxInst)
        // var regconfig:Array[Array[Int]] = Array.ofDim[Int](maxInst, dataWidth)
        // var regconfigTag:Array[Int] = new Array[Int](maxInst)

        // var remuPEInst:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, perInst,dataWidth*2)
        // var remuPERedist:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, 4*perInst,perInst+tagWidth)
        // var remuPEOut:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, perInst,outSize*3)

        
        var regCnt=0
        var myCGRA:Array[Int] = new Array[Int](this.CGRASize)
        for(i<-0 to this.maxInst-1){
            this.InstRemuNo(i)=0
        }
        for(i <- 0 to this.CGRASize-1){
            myCGRA(i)=0
            for(j <- 0 to this.perInst-1){
                for(k <- 0 to this.dataWidth*2-1){
                    this.remuPEInst(i)(j)(k)=0
                }
                for(k <- 0 to this.tagWidth-1){
                    this.remuPETag(i)(j)(k)=0
                }
                for(k <- 0 to this.outSize*3-1){
                    this.remuPEOut(i)(j)(k)=0
                }
                for(k<-0 to 3){
                    this.remuPEData(i)(j)(k)=0
                }
            }
            for(j <- 0 to 4*this.perInst-1){
                for(k <- 0 to this.perInst+this.tagWidth-1){
                    this.remuPERedist(i)(j)(k)=0
                }
            }
        }
        for(i <- 0 to this.CGRASize-1){
            for(j <- 0 to this.tagWidth-1){
                this.remuRegTags(i)(j) = 0
            }
            for(j <- 0 to 4+this.tagWidth-1){
                this.remuRegV(i)(j) = 0
            }
            remuRegValid(i)=0
        }


        for(i <- 0 to this.totalInstNum-1){
            //scan the instruction placement and choose the CGRA pe to place 
            this.InstRemuNo(i)=myCGRA(this.finalplacements(i))
            for(j <- 0 to this.dataWidth*2-1){
                if(myCGRA(this.finalplacements(i))>=16){
                    println("wtf at ",this.finalplacements(i))
                }
                this.remuPEInst(this.finalplacements(i))(myCGRA(this.finalplacements(i)))(j)=this.finalInstForRemu(i)(j)

            }
            for(j <- 0 to this.tagWidth-1){
                val binaryStr = (i+1).toBinaryString.reverse.padTo(this.tagWidth, '0')
                
                this.remuPEInst(this.finalplacements(i))(myCGRA(this.finalplacements(i)))(j)=binaryStr(j) - '0'
            }
            for(j <- 0 to this.outSize-1){
                this.remuPEOut(this.finalplacements(i))(myCGRA(this.finalplacements(i)))(j)=this.remuoutSetD0(i)(j)
                
            }
            for(j <- 0 to this.outSize-1){
                this.remuPEOut(this.finalplacements(i))(myCGRA(this.finalplacements(i)))(j+this.outSize)=this.remuoutSetD1(i)(j)
                
            }
            for(j <- 0 to this.outSize-1){
                this.remuPEOut(this.finalplacements(i))(myCGRA(this.finalplacements(i)))(j+2*this.outSize)=this.remuoutSetD2(i)(j)
                
            }
            for(j <- 0 to 4-1){
                this.remuPEData(this.finalplacements(i))(myCGRA(this.finalplacements(i)))(j)=this.remudataNum(i)(j)
                
            }

            myCGRA(this.finalplacements(i))=myCGRA(this.finalplacements(i))+1
        }

        var seenTags:Array[Array[Int]]=Array.ofDim[Int](this.perInst, 2)
        var seenTagsReg:Array[Array[Int]]=Array.ofDim[Int](this.CGRASize, 3)
        var regSize=0
        for(i <- 0 to this.CGRASize-1){
                
            seenTagsReg(i)(0)=0//0 is the tag
            seenTagsReg(i)(1)=0//1 is the exist flag
            seenTagsReg(i)(2)=this.perInst+1//2 is the exist flag
        }
        for(k <- 0 to this.CGRASize-1){
            //for each pe scan its insts
            var dnums=0
            var dnumsReg=0
            for(i <- 0 to this.perInst-1){
                seenTags(i)(0)=0//0 is the tag
                seenTags(i)(1)=0//1 is the number
                
            }
            
            for(i <- 0 to this.totalInstNum-1){
                //scan the instruction to revise the inputs redistributed
                if(this.finalplacements(i)==k){
                    // var remuPERedist:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, 4*perInst,perInst+tagWidth)
                    //first scan d0
                    for(j<-0 to this.remuTagresolveNum(i)-1){
                        if(this.remuTagresolve1(i)(j)==1&&j!=this.whichReg(i)){
                            var df=0
                            var fnum=0
                            for(m<-0 to this.perInst-1){
                                if(seenTags(m)(0)==this.remuTagresolve0(i)(j)){
                                    df=1
                                    fnum=m
                                }
                            }
                            if(df==0){
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                dnums=dnums+1
                                
                            }
                            if(df==0){
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    // var o=this.remuTagresolve0(i)(j)
                                    // println(s"binary $binaryStr, original $o")
                                    this.remuPERedist(k)(fnum)(m) = binaryStr(m) - '0'
                                }
                            }
                            for(m<-0 to this.tagWidth-1){
                                    
                                val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                // var o=this.remuTagresolve0(i)(j)
                                // println(s"binary $binaryStr, original $o")
                                this.remuPERedist(k)(fnum)(m) = binaryStr(m) - '0'
                            }
                            this.remuPERedist(k)(fnum)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                        else if(this.remuTagresolve1(i)(j)==1){//is a reg now ensure that no reg is found
                            var fnum=0
                            if(seenTagsReg(k)(1)==0){
                                seenTagsReg(k)(0)=this.remuTagresolve0(i)(j)
                                seenTagsReg(k)(1)=1
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                seenTagsReg(k)(2)=fnum
                                dnums=dnums+1
                                
                                
                                this.remuRegValid(k)=1
                                this.remuRegV(k)(0+this.tagWidth)=1
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum)(m) = binaryStr(m) - '0'
                                    this.remuRegTags(k)(m) = binaryStr(m) - '0'
                                    this.remuRegV(k)(m) = binaryStr(m) - '0'
                                }
                            }
                            else{
                                fnum=seenTagsReg(k)(2)
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = seenTagsReg(k)(0).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum)(m) = binaryStr(m) - '0'
                                    this.remuRegTags(k)(m) = binaryStr(m) - '0'
                                    this.remuRegV(k)(m) = binaryStr(m) - '0'
                                }
                                this.remuRegV(k)(0+this.tagWidth)=1
                            }
                            this.remuPERedist(k)(fnum)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                    }
                }

            }
        }

        for(i <- 0 to this.CGRASize-1){
            seenTagsReg(i)(2)=this.perInst+1//2 is the exist flag
        }
        for(k <- 0 to this.CGRASize-1){
            //for each pe scan its insts
            var dnums=0
            for(i <- 0 to this.perInst-1){
                seenTags(i)(0)=0//0 is the tag
                seenTags(i)(1)=0//1 is the number
            }
            
            for(i <- 0 to this.totalInstNum-1){
                //scan the instruction to revise the inputs redistributed
                if(this.finalplacements(i)==k){
                    // var remuPERedist:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, 4*perInst,perInst+tagWidth)
                    //then scan d1
                    for(j<-0 to this.remuTagresolveNum(i)-1){
                        if(this.remuTagresolve1(i)(j)==2&&j!=this.whichReg(i)){
                            var df=0
                            var fnum=0
                            for(m<-0 to this.perInst-1){
                                if(seenTags(m)(0)==this.remuTagresolve0(i)(j)){
                                    df=1
                                    fnum=m
                                }
                            }
                            if(df==0){
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                dnums=dnums+1
                                
                            }
                            if(df==0){
                                for(m<-0 to this.tagWidth-1){
                                    assert(this.remuTagresolve0(i)(j)!=0)
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum+this.perInst)(m) = binaryStr(m) - '0'
                                    // if(i==16){
                                    //     println(s"16 find")
                                    //     println(binaryStr)
                                    // }
                                }
                            }
                            for(m<-0 to this.tagWidth-1){
                                assert(this.remuTagresolve0(i)(j)!=0)
                                val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                this.remuPERedist(k)(fnum+this.perInst)(m) = binaryStr(m) - '0'
                                // if(i==16){
                                //         println(s"16 find")
                                //         println(binaryStr)
                                //     }
                            }
                            this.remuPERedist(k)(fnum+this.perInst)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                        else if(this.remuTagresolve1(i)(j)==2){//is a reg now ensure that no reg is found
                            var fnum=0
                            if(seenTagsReg(k)(1)==0){
                                seenTagsReg(k)(0)=this.remuTagresolve0(i)(j)
                                seenTagsReg(k)(1)=1
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                seenTagsReg(k)(2)=fnum
                                dnums=dnums+1
                                this.remuRegValid(k)=1
                                this.remuRegV(k)(1+this.tagWidth)=1
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum+this.perInst)(m) = binaryStr(m) - '0'

                                    this.remuRegTags(k)(m) = binaryStr(m) - '0'
                                    this.remuRegV(k)(m) = binaryStr(m) - '0'
                                }
                            }
                            else{
                                if(seenTagsReg(k)(2)==this.perInst+1){
                                    seenTags(dnums)(0)=seenTagsReg(k)(0)
                                    seenTags(dnums)(1)=dnums
                                    fnum=dnums
                                    seenTagsReg(k)(2)=fnum
                                    dnums=dnums+1
                                    this.remuRegV(k)(1+this.tagWidth)=1
                                    for(m<-0 to this.tagWidth-1){
                                        
                                        val binaryStr = seenTagsReg(k)(0).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                        // var o=seenTagsReg(k)(0)
                                        // println(s"binary $binaryStr, original $o")
                                        this.remuPERedist(k)(fnum+this.perInst)(m) = binaryStr(m) - '0'
                                    }
                                }
                                else{
                                    fnum=seenTagsReg(k)(2)
                                    for(m<-0 to this.tagWidth-1){
                                        
                                        val binaryStr = seenTagsReg(k)(0).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                        // var o=seenTagsReg(k)(0)
                                        // println(s"binary $binaryStr, original $o")
                                        this.remuPERedist(k)(fnum+this.perInst)(m) = binaryStr(m) - '0'
                                    }
                                    this.remuRegV(k)(1+this.tagWidth)=1
                                }
                            }
                            this.remuPERedist(k)(fnum+this.perInst)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                    }
                }

            }
        }

        for(i <- 0 to this.CGRASize-1){
            seenTagsReg(i)(2)=this.perInst+1//2 is the exist flag
        }
        for(k <- 0 to this.CGRASize-1){
            //for each pe scan its insts
            var dnums=0
            for(i <- 0 to this.perInst-1){
                seenTags(i)(0)=0//0 is the tag
                seenTags(i)(1)=0//1 is the number
            }
            
            for(i <- 0 to this.totalInstNum-1){
                //scan the instruction to revise the inputs redistributed
                if(this.finalplacements(i)==k){
                    // var remuPERedist:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, 4*perInst,perInst+tagWidth)
                    //then scan d2
                    for(j<-0 to this.remuTagresolveNum(i)-1){
                        if(this.remuTagresolve1(i)(j)==3&&j!=this.whichReg(i)){
                            var df=0
                            var fnum=0
                            for(m<-0 to this.perInst-1){
                                if(seenTags(m)(0)==this.remuTagresolve0(i)(j)){
                                    df=1
                                    fnum=m
                                }
                            }
                            if(df==0){
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                dnums=dnums+1
                                
                            }
                            if(df==0){
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum+2*this.perInst)(m) = binaryStr(m) - '0'
                                    // if(i==16){
                                    //     println(s"16 find")
                                    //     println(binaryStr)
                                    // }
                                }
                            }
                            for(m<-0 to this.tagWidth-1){
                                
                                val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                this.remuPERedist(k)(fnum+2*this.perInst)(m) = binaryStr(m) - '0'
                                // if(i==16){
                                //     println(s"16 find")
                                //     println(binaryStr)
                                // }
                            }
                            this.remuPERedist(k)(fnum+2*this.perInst)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                        else if(this.remuTagresolve1(i)(j)==3){//is a reg now ensure that no reg is found
                            var fnum=0
                            if(seenTagsReg(k)(1)==0){
                                seenTagsReg(k)(0)=this.remuTagresolve0(i)(j)
                                seenTagsReg(k)(1)=1
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                seenTagsReg(k)(2)=fnum
                                dnums=dnums+1
                                
                                this.remuRegValid(k)=1
                                this.remuRegV(k)(2+this.tagWidth)=1
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum+2*this.perInst)(m) = binaryStr(m) - '0'
                                    this.remuRegTags(k)(m) = binaryStr(m) - '0'
                                    this.remuRegV(k)(m) = binaryStr(m) - '0'
                                }
                            }
                            else{
                                if(seenTagsReg(k)(2)==this.perInst+1){
                                    seenTags(dnums)(0)=seenTagsReg(k)(0)
                                    seenTags(dnums)(1)=dnums
                                    fnum=dnums
                                    seenTagsReg(k)(2)=fnum
                                    dnums=dnums+1

                                    this.remuRegV(k)(2+this.tagWidth)=1
                                    
                                    for(m<-0 to this.tagWidth-1){
                                        
                                        val binaryStr = seenTagsReg(k)(0).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                        this.remuPERedist(k)(fnum+2*this.perInst)(m) = binaryStr(m) - '0'
                                    }
                                }
                                else{
                                    
                                    fnum=seenTagsReg(k)(2)
                                    for(m<-0 to this.tagWidth-1){
                                        
                                        val binaryStr = seenTagsReg(k)(0).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                        this.remuPERedist(k)(fnum+2*this.perInst)(m) = binaryStr(m) - '0'
                                    }
                                    this.remuRegV(k)(2+this.tagWidth)=1
                                }
                            }
                            this.remuPERedist(k)(fnum+2*this.perInst)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                    }
                }

            }
        }

        for(i <- 0 to this.CGRASize-1){
            seenTagsReg(i)(2)=this.perInst+1//2 is the exist flag
        }
        for(k <- 0 to this.CGRASize-1){
            //for each pe scan its insts
            var dnums=0
            for(i <- 0 to this.perInst-1){
                seenTags(i)(0)=0//0 is the tag
                seenTags(i)(1)=0//1 is the number
            }
            
            for(i <- 0 to this.totalInstNum-1){
                //scan the instruction to revise the inputs redistributed
                if(this.finalplacements(i)==k){
                    // var remuPERedist:Array[Array[Array[Int]]] = Array.ofDim[Int](CGRASize, 4*perInst,perInst+tagWidth)
                    //then scan d2
                    for(j<-0 to this.remuTagresolveNum(i)-1){
                        if(this.remuTagresolve1(i)(j)==4&&j!=this.whichReg(i)){
                            var df=0
                            var fnum=0
                            for(m<-0 to this.perInst-1){
                                if(seenTags(m)(0)==this.remuTagresolve0(i)(j)){
                                    df=1
                                    fnum=m
                                }
                            }
                            if(df==0){
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                dnums=dnums+1
                                
                            }
                            if(df==0){
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum+3*this.perInst)(m) = binaryStr(m) - '0'
                                }
                            }
                            for(m<-0 to this.tagWidth-1){
                                
                                val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                this.remuPERedist(k)(fnum+3*this.perInst)(m) = binaryStr(m) - '0'
                            }
                            this.remuPERedist(k)(fnum+3*this.perInst)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                        else if(this.remuTagresolve1(i)(j)==4){//is a reg now ensure that no reg is found
                            var fnum=0
                            if(seenTagsReg(k)(1)==0){
                                seenTagsReg(k)(0)=this.remuTagresolve0(i)(j)
                                seenTagsReg(k)(1)=1
                                seenTags(dnums)(0)=this.remuTagresolve0(i)(j)
                                seenTags(dnums)(1)=dnums
                                fnum=dnums
                                seenTagsReg(k)(2)=fnum
                                dnums=dnums+1

                                this.remuRegValid(k)=1
                                this.remuRegV(k)(3+this.tagWidth)=1
                                
                                for(m<-0 to this.tagWidth-1){
                                    
                                    val binaryStr = this.remuTagresolve0(i)(j).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                    this.remuPERedist(k)(fnum+3*this.perInst)(m) = binaryStr(m) - '0'
                                    this.remuRegTags(k)(m) = binaryStr(m) - '0'
                                    this.remuRegV(k)(m)=binaryStr(m) - '0'
                                }
                            }
                            else{
                                if(seenTagsReg(k)(2)==this.perInst+1){
                                    seenTags(dnums)(0)=seenTagsReg(k)(0)
                                    seenTags(dnums)(1)=dnums
                                    fnum=dnums
                                    seenTagsReg(k)(2)=fnum
                                    dnums=dnums+1

                                    this.remuRegV(k)(3+this.tagWidth)=1
                                    
                                    for(m<-0 to this.tagWidth-1){
                                        
                                        val binaryStr = seenTagsReg(k)(0).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                        this.remuPERedist(k)(fnum+3*this.perInst)(m) = binaryStr(m) - '0'
                                    }
                                }
                                else{
                                    fnum=seenTagsReg(k)(2)
                                    for(m<-0 to this.tagWidth-1){
                                        
                                        val binaryStr = seenTagsReg(k)(0).toBinaryString.reverse.padTo(this.tagWidth, '0')
                                        this.remuPERedist(k)(fnum+3*this.perInst)(m) = binaryStr(m) - '0'
                                    }
                                    this.remuRegV(k)(3+this.tagWidth)=1
                                }
                            }
                            this.remuPERedist(k)(fnum+3*this.perInst)(this.tagWidth+this.InstRemuNo(i))=1
                        }
                    }
                }

            }
        }

        //then process the register array
        // var remuRegTags:Array[Array[Int]] = Array.ofDim[Int](maxInst,tagWidth)
        // var remuRegV:Array[Array[Int]] = Array.ofDim[Int](maxInst, 4+tagWidth)
        
        return 0
    }
}









object TestMapParse extends App {
    val spatial = false
    if (spatial) {
        val appName = "kmpbk.ll"
        val basePath = ".\\"
        val witdh=17
        val codeEmit = new CodeEmit(appName, 32, s"${basePath}${appName}.dot.map",witdh, witdh*witdh,1, 2048)
        codeEmit.CGRAGen(1,1)
        val tmp = codeEmit.parseDot(s"${basePath}${appName}.dot", 32)
        val result = codeEmit.parseMapping(s"${basePath}${appName}.dot.map")
        codeEmit.analyzeBackendPass0()
        codeEmit.analyzeBackendPass1()
        codeEmit.analyzeBackendPass2()
        codeEmit.verifications()
        // codeEmit.Backend2Remulation()
        // codeEmit.analyzeRemulationPass0()
        // codeEmit.analyzeRemulationPass1()
        // codeEmit.debugPrintInst()
        println("Parse and analyze successfully!")
        
        
        val writer = new PrintWriter(new File(s"${appName}_arrays_output.txt"))

        def writeArray2D(name: String, arr: Array[Array[Int]]): Unit = {
        writer.println(s"$name:")
        arr.zipWithIndex.foreach { case (row, i) =>
            writer.println(s"$name($i): " + row.mkString(", "))
        }
        writer.println()
        }

        def writeArray1D(name: String, arr: Array[Int]): Unit = {
            writer.println(s"$name:")
            arr.zipWithIndex.foreach { case (value, i) =>
                writer.println(s"$name($i): $value")
            }
            writer.println()
        }


        // Access and write arrays from codeEmit
        writeArray2D("outSetD0", codeEmit.outSetD0)
        writeArray2D("outSetD1", codeEmit.outSetD1)
        writeArray2D("outSetP0", codeEmit.outSetP0)
        writeArray2D("outSetP1", codeEmit.outSetP1)
        writeArray1D("finalplacements", codeEmit.finalplacements)
        
        writeArray2D("finalInstArray", codeEmit.finalInstArray)

        writer.close()
        println("Arrays written to arrays_output.txt")

        for(i<-0 to codeEmit.maxInst-1){
                for(j<- 0 to codeEmit.outSize-1){
                    print(codeEmit.outSetD0(i)(j))
                }
                println()

        }
    } else {
        var appName = "./benchmark/ataxno.ll"  // 
        appName=args.lift(0).getOrElse("mNew.ll")
        var tesss=args.lift(0).getOrElse("?????")
        val basePath = ".\\"
        var witdh=8
        witdh=Integer.parseInt(args.lift(1).getOrElse("6"))
        var numInst=16
        numInst=Integer.parseInt(args.lift(2).getOrElse("16"))
        var tagWidth=16
        tagWidth=Integer.parseInt(args.lift(3).getOrElse("16"))
        var outSize=36;
        outSize=Integer.parseInt(args.lift(4).getOrElse("9"))
        val codeEmit = new CodeEmit(appName, 32, s"${basePath}${appName}.dot.map",witdh, witdh*witdh, numInst, 2048,outSize1=outSize)
        codeEmit.CGRAGen(0,1)
        val tmp = codeEmit.parseDot(s"${basePath}${appName}.dot", 32)
        val result = codeEmit.parseMapping(s"${basePath}${appName}.dot.map")
        codeEmit.analyzeBackendPass0()
        codeEmit.analyzeBackendPass1()
        codeEmit.analyzeBackendPass2()
        codeEmit.verifications()
        codeEmit.Backend2Remulation()
        codeEmit.analyzeRemulationPass0()
        codeEmit.analyzeRemulationPass1()
        // codeEmit.debugPrintInst()
        println("Parse and analyze successfully!")
        
        
        val writer = new PrintWriter(new File(s"${appName}_t_arrays_output.txt"))

        def writeArray3D(name: String, arr: Array[Array[Array[Int]]]): Unit = {
        writer.println(s"$name:")
        arr.zipWithIndex.foreach { case (pe, i) =>
            writer.println(s"$name($i):")
            pe.zipWithIndex.foreach { case (inst, k) =>
            writer.println(s"  $name($i)($k): " + inst.mkString(", "))
            }
        }
        writer.println()
        }

        def writeRemuPEInstCode(
            name: String,
            arr: Array[Array[Array[Int]]],
            instructionWidth: Int
        ): Unit = {
        writer.println(s"$name Instructions:")
        arr.zipWithIndex.foreach { case (pe, i) =>
            writer.println(s"$name($i) Instructions:")
            pe.zipWithIndex.foreach { case (inst, k) =>
            writer.println(s"  $name($i)($k): " + inst.mkString(", "))
            // val code = inst.slice(0, instructionWidth) 
            // val codeStr = code.mkString(", ")
            // writer.println(s"  $name($i)($k): " + codeStr)
            
            }
            writer.println()
        }
        }

        // Routing: D0, D1, D2, D3
        def writeRemuPERouting(
            name: String,
            arr: Array[Array[Array[Int]]],
            instructionWidth: Int,
            outNum: Int
        ): Unit = {
        for (port <- 0 until 4) {
            writer.println(s"$name Routing D$port:")
            arr.zipWithIndex.foreach { case (pe, i) =>
            writer.println(s"$name($i) Routing D$port:")
            pe.zipWithIndex.foreach { case (inst, k) =>
                val routingBits = inst.slice(port*outNum, (port+1)*outNum)
                val bitStr = routingBits.mkString(", ")
                writer.println(s"  $name Routing D$port($i)($k): " + bitStr)
            }
            }
            writer.println()
        }
        }

        def writeRemuPERedist(
            name: String,
            arr: Array[Array[Array[Int]]],
            CGRASize: Int,
            perInst: Int = numInst,
            tagWidth: Int = 16,
            subInstructionNum: Int = numInst
        ): Unit = {
        writer.println(s"$name:")

        for (i <- 0 until CGRASize) {
            writer.println(s"$name($i):")

            for (k <- 0 until subInstructionNum) {
            // Data0~Data3
            for (d <- 0 until 4) {
                val idx = k + d * subInstructionNum
                val inst = arr(i)(idx)

                val tags = inst.slice(0, tagWidth)

                val forward = inst.slice(tagWidth, tagWidth + subInstructionNum)

                writer.println(s"  PE($i) Inst($k) Data$d:")
                writer.println(s"$name Tags (tagWidth=$tagWidth): " + tags.mkString(", "))
                writer.println(s"$name Forward bitvector (len=$subInstructionNum): " + forward.mkString(", "))
            }
            }
        }
        writer.println()
        }

        def writeRemuRegV(
            name: String,
            arr: Array[Array[Int]],
            CGRASize: Int,
            tagWidth: Int = 16
        ): Unit = {
        writer.println(s"$name:")

        for (i <- 0 until CGRASize) {
            val row = arr(i)

            val tags = row.slice(0, tagWidth)

            val inputs = row.slice(tagWidth, tagWidth + 4)

            writer.println(s"$name($i):")
            writer.println(s"$name Tags (tagWidth=$tagWidth): " + tags.mkString(", "))

            val inputNames = Seq("d0", "d1", "d2", "d3")

            writer.println(s"$name Inputs (4 bits, d0~d3): " + inputs.mkString(", "))
        }

        writer.println()
        }




        def writeArray2D(name: String, arr: Array[Array[Int]]): Unit = {
        writer.println(s"$name:")
        arr.zipWithIndex.foreach { case (row, i) =>
            writer.println(s"$name($i): " + row.mkString(", "))
        }
        writer.println()
        }

        def writeArray1D(name: String, arr: Array[Int]): Unit = {
            writer.println(s"$name:")
            arr.zipWithIndex.foreach { case (value, i) =>
                writer.println(s"$name($i): $value")
            }
            writer.println()
        }


        // Access and write arrays from codeEmit
        // writeArray2D("outSetD0", codeEmit.outSetD0)
        // writeArray2D("outSetD1", codeEmit.outSetD1)
        // writeArray2D("outSetP0", codeEmit.outSetP0)
        // writeArray2D("outSetP1", codeEmit.outSetP1)
        // writeArray1D("finalplacements", codeEmit.finalplacements)
        
        // writeArray2D("finalInstArray", codeEmit.finalInstArray)

        writeRemuPEInstCode("remuPEInst", codeEmit.remuPEInst, instructionWidth = 64)
        writeRemuPERouting("remuPEOut", codeEmit.remuPEOut, instructionWidth = 64, outNum = outSize)

        writeArray3D("remuPEData", codeEmit.remuPEData)

        writeRemuPERedist("remuRedist", codeEmit.remuPERedist, CGRASize = witdh*witdh)

        writeRemuRegV("remuRegV", codeEmit.remuRegV, CGRASize = witdh*witdh)
        writeArray1D("remuRegValid", codeEmit.remuRegValid)
        writeArray2D("remuRegTags", codeEmit.remuRegTags)

        writer.close()
        println("Arrays written to arrays_output.txt")

        for(i<-0 to codeEmit.maxInst-1){
                for(j<- 0 to codeEmit.outSize-1){
                    print(codeEmit.outSetD0(i)(j))
                }
                println()

        }
        println(appName)
        println(tesss)
    }

}
}
