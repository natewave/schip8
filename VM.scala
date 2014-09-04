/*****************************************************************************
 *                        Schip8:   A Chip-8 Emulator
 *           Copyright (C) 2014, S. Nizar <nisehl at gmail dot com>
 *
 * VM.scala:
 *  Structures to manipulate chip-8 instructions set
 *****************************************************************************/
package schip8

object Chip8Specs {
  /* allocate 4k memory                          */
  val memorySize: Int = 4096

  /* all chip-8 programs start at location 0x200 */
  val memoryStartLocation: Int = 0x200

  val CPUSpead: Int = 100 // Hz
}

case class VMState(
  memory: Memory,
  pc: Int = Chip8Specs.memoryStartLocation, /* Program counter      */
  I: Short = 0,                             /* I - 16-bit register  */
  V: Array[Byte] = new Array[Byte](16),     /* V - 8-bit registers  */
  screen: Screen
) {
  def apply(memory: Memory, screen: Screen): VMState =
    VMState(
      memory=memory,
      pc= Chip8Specs.memoryStartLocation,
      I=0,
      V=new Array[Byte](16),
      screen=screen
    )
}

case class Memory(size: Int, start: Int) {
  val locations = new Array[Short](size)
}

object VM {
  /* Load a program in Memory */
  def loadProgram(program: Array[Short]): Memory = {
    val memory = Memory(Chip8Specs.memorySize, Chip8Specs.memoryStartLocation)
    for (i <- 0 until program.length - 1)
      memory.locations(Chip8Specs.memoryStartLocation + i) = program(i)
    memory
  }

  def step(vmState: VMState): VMState = Instructions.executeNext(vmState)

  /* Display n-bytes sprite starting at memory `address` at (x, y)
     returns a tuple for the new VMState and a boolean for collision */
  def drawSprite(
    x: Byte,
    y: Byte,
    address: Short,
    nbytes: Byte,
    vmState: VMState
  ): Boolean = {
    def walkBits(bits: Short, bit: Int, line: Int, collision: Boolean): Boolean = {
      if(bit < 0) collision else {
        if( (bits & 1) == 1) {
          if( !vmState.screen.display.xorPixel(x + bit, y + line) )
            walkBits( (bits >> 1).toShort, bit - 1, line, true)
        }
        walkBits( (bits >> 1).toShort, bit - 1, line, collision)
      }
    }

    /* Walk the horizontal lines                                                              */
    def walkLines(line: Int, lines: Int, collision: Boolean): Boolean = {
      if(line >= lines ) collision else {
        val bits = vmState.memory.locations(address + line) /* Get the sprite line bits to draw */
        val collision = walkBits(bits, 7, line, false) /* Walk the bits                         */
        walkLines(line + 1, lines, collision)
      }
    }
    
    walkLines(0, nbytes, false)
  }

  def run(initState: VMState) {
    val newVMState = step(initState)
    Thread sleep (1000 / Chip8Specs.CPUSpead) /* 50 Hz = 50 cycles per second */
    run(newVMState)
  }
}