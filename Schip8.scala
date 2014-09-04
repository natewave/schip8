/*****************************************************************************
 *                        Schip8:   A Chip-8 Emulator
 *           Copyright (C) 2014, S. Nizar <nisehl at gmail dot com>
 *
 * Schip8.scala:
 *  Program's main entry
 *****************************************************************************/

package schip8

object Schip8 {

  /* Load rom from roms directory */
  def loadRom(path: String): Array[Short] = {
    import scala.io.{Source, BufferedSource}
    val source: BufferedSource = Source.fromFile(path, "ISO-8859-1")
    val program: Array[Short] = source.map((_.toShort)).toArray
    source.close()
    program
  }

  def main(args: Array[String]) {
    /* Load program into memory */
    val rom = if (args.size > 0) args(0) else "roms/MAZE"
    print("Loading ROM...")
    val program = loadRom("roms/MAZE")
    val memory = VM.loadProgram(program)
    println("DONE!")

    /* Initialize screen        */
    val screen = Screen(640, 320)
    screen.startup(args)
    screen.display.clear()

    /* Initialize VM            */
    val vmState = VMState(memory=memory, screen=screen)
    VM.run(vmState)
  }
}