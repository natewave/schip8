/*****************************************************************************
 *                        Schip8:   A Chip-8 Emulator
 *           Copyright (C) 2014, S. Nizar <nisehl at gmail dot com>
 *
 * Instructions.scala:
 *  Opcodes and operands implementations for executing
 *  instructions
 *****************************************************************************/

package schip8

object Opcodes {
  /* 1nnn - Jump to location nnn */
  def onennn(o: Operands, vmState: VMState): VMState = vmState.copy(pc= o.nnn)

  /* 3xkk - Skip next instruction if Vx = kk */
  def threexkk(o: Operands, vmState: VMState): VMState =
    if (vmState.V(o.x) == o.kk) vmState.copy(pc = vmState.pc + 2) else vmState

  /* 6xkk - Set Vx = kk */
  def sixxkk(o: Operands, vmState: VMState): VMState = {
    val VV = vmState.V
    VV(o.x) = o.kk
    vmState.copy(V=VV)
  }

  /* 7xkk - Set Vx = Vx + kk */
  def sevenxkk(o: Operands, vmState: VMState): VMState = {
    val VV = vmState.V
    VV(o.x) = (VV(o.x) + o.kk).toByte
    vmState.copy(V=VV)
  }

  /* Annn - Set I = nnn */
  def annn(o: Operands, vmState: VMState): VMState = vmState.copy(I=o.nnn)

  /* Cxkk - Set Vx = random byte AND kk. */
  def cxkk(o: Operands, vmState: VMState): VMState = {
    val randomByte = new Array[Byte](1)
    scala.util.Random.nextBytes(randomByte)
    val VV = vmState.V
    VV(o.x) = (randomByte(0) & o.kk).toByte
    vmState.copy(V=VV)
  }

  /* Dxyn - Display n-byte sprite starting at memory location I at (Vx, Vy).
     Set VF = collision */
  def dxyn(o: Operands, vmState: VMState): VMState = {
    val collision = VM.drawSprite(vmState.V(o.x), vmState.V(o.y), vmState.I, o.n, vmState)
    val VV = vmState.V
    if (collision) VV(0xF) = 1 else VV(0xF) = 0
    vmState.copy(V=VV)
  }  

  /* opcodes execution */
  def execute(vmState: VMState): VMState = {
    /* a chip-8 instruction is 16-bit */
    val instruction = (
      (vmState.memory.locations(vmState.pc) << 8) /* Get first byte and shift it << 8 */
      + vmState.memory.locations(vmState.pc + 1)  /* concatenate with second byte     */
    )

    val opcode    = (instruction & 0xF000)
    val operands  = Operands(instruction)
    println("Executing Instruction..." + instruction.toHexString)

    opcode match {
      case 0x1000 =>
        /* 1nnn - Jump to location nnn */
        Opcodes.onennn(operands, vmState)
      case 0x3000 =>
        /* 3xkk - Skip next instruction if Vx = kk */
        Opcodes.threexkk(operands, vmState)
      case 0x6000 =>
        /* 6xkk - Set Vx = kk */
        Opcodes.sixxkk(operands, vmState)
      case 0x7000 =>
        /* 7xkk - Set Vx = Vx + kk */
        Opcodes.sevenxkk(operands, vmState)
      case 0xA000 =>
        /* Annn - Set I = nnn */
        Opcodes.annn(operands, vmState)

      case 0xC000 =>
        /* Cxkk - Set Vx = random byte AND kk. */
        Opcodes.cxkk(operands, vmState)

      case 0xD000 =>
        /* Dxyn - Display n-byte sprite starting at memory location I at (Vx, Vy).
           Set VF = collision */
        Opcodes.dxyn(operands, vmState)
      case _ =>
        vmState
    }
  }
}

/* Extract operands from instruction */
case class Operands(instruction: Int) {
  def nnn       = (instruction & 0x0FFF).toShort
  def x         = ((instruction & 0x0F00) >> 8).toByte
  def y         = ((instruction & 0x00F0) >> 4).toByte
  def n         = ((instruction & 0x000F)).toByte
  def kk        = (instruction & 0x00FF).toByte
}

object Instructions {
  def executeNext(vmState: VMState): VMState = {
    /* Execute instruction at pc and get new VM state */
    val newVmState = Opcodes.execute(vmState)

    /* Update program counter.
       Instructions in chip-8 are 2 bytes long */
    newVmState.copy(pc = newVmState.pc + 2)
  }
}
