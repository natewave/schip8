/*****************************************************************************
 *                        Schip8:   A Chip-8 Emulator
 *           Copyright (C) 2014, S. Nizar <nisehl at gmail dot com>
 *
 * Display.scala:
 *  Screen for displaying our program
 *****************************************************************************/
package schip8

import swing.{Panel, MainFrame, SimpleSwingApplication}
import java.awt.{Color, Graphics2D, Dimension}

/* A display of 64x32 pixels scaled to the screen dimensions */
case class Display(
  width: Int,
  height: Int,
  screenWidth: Int,
  screenHeight: Int,
  var pixels: Array[Array[Color]]
) extends Panel {

  /* The scale of one display pseudo-pixel
  A terminal screen of 650px width = one pseudo-pixel is 10px width */
  val xScale = screenWidth / width
  val yScale = screenHeight / height
  
  /* XOR a pixel at (x, y)
  returns true if the pixel was turned on */
  def xorPixel(x: Int, y: Int): Boolean = {
    // Wrap around vertically
    if (x > width) {
      xorPixel(x - width, y)
    } else if (x < 0) {
      xorPixel(x + width, y)
    }
    
    // Wrap around horizontally
    if (y > height) {
      xorPixel(x, y -height)
    } else if (y < 0) {
      xorPixel(x, y + height)
    }
    
    // Get the pixel state
    val active = pixels(x)(y) match {
      case c: Color if c == Color.WHITE => true
      case _ => false
    }

    if (active) {
      // Clear pixel
      pixels(x)(y) = Color.BLACK
    } else {
      // Draw pixel
      pixels(x)(y) = Color.WHITE
    }

    repaint()

    // Return pixel state
    active
    
  }
  
  def draw(pixels: Array[Array[Color]], g: Graphics2D) {

    for {
      x <- 0 until pixels.length
      y <- 0 until pixels(x).length
      x1 = (x * xScale).toInt
      y1 = (y * yScale).toInt
      x2 = ((x + 1) * xScale).toInt
      y2 = ((y + 1) * yScale).toInt
    } {
      pixels(x)(y) match {
        case c: Color => g.setColor(c)
        case _ => g.setColor(Color.WHITE)
      }
      g.fillRect(x1, y1, x2 - x1, y2 - y1)
    }
  }
  
  override def paintComponent(g: Graphics2D) {
    draw(pixels, g)
  }
  
  def clear() {
    pixels = pixels.map(p => p.map(c => Color.BLACK))
    repaint()
  }
}

/* We'll store Terminal dimensions here */
case class Screen(width: Int, height: Int) extends SimpleSwingApplication {

  val chip8width = 64
  val chip8height = 32
  val pixels = Array.ofDim[Color](chip8width, chip8height)

  val display = new Display(chip8width, chip8height, width, height, pixels.map(p => p.map(c => Color.BLACK))) {
    preferredSize = new Dimension(screenWidth, screenHeight)
  }

  def top = new MainFrame {
    title     = "Chip-8 Emulator"
    contents = display
  }
}
