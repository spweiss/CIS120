import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.MouseEvent;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import javax.swing.JPanel;

@SuppressWarnings("serial")
public abstract class Space extends JPanel {
    
    public Space() {
    }
    
    /**
     * Creates and scales an image based on a file to be read in and a provided
     * image to save it to. This method is inherited by Mine and Blank and used
     * to create the image files to be swapped when various actions are taken.
     * 
     * @param filename
     * The file path of the desired image.
     * 
     * @param target
     * The provided image object to save the image to.
     */
    public Image makeImage(String filename, Image target) {
        try {
            if (target == null) {
                Image temp = ImageIO.read(new File(filename));
                target = temp.getScaledInstance(20, 20, Image.SCALE_SMOOTH);
            }
        } catch (IOException e) {
            System.out.println("Internal File Error:" + e.getMessage());
        }
        return target;
    }
    
    /**
     * Default draw method that provides how the Space should be drawn 
     * in the GUI. This method does not draw anything. Blank and Mine 
     * override this method based on their respective appearances.
     * 
     * @param g 
     * The <code>Graphics</code> context used for drawing the object.
     * Gives the context in which the object should be drawn (a canvas,
     * a frame, etc.)
     */
    public void draw(Graphics g) {
    }
    
    /**
     * Default handle method so that GameBoard can call handle on Space
     * when a MouseEvent is triggered.  This method is overridden by Blank
     * and Mine.
     */
    public void handle(MouseEvent e) {
    }
    
    /**
     * Default setCount method so that GameBoard can call setCount on Space
     * when it tabulates the number of mines surrounding a Space. This method
     * is overridden by Blank and Mine.
     */
    public void setCount(int count) {
    }
    
    /**
     * Default getCount method so that GameBoard can call getCount on Space
     * when it sets up operations for cascades. This method is overridden by
     * Blank and Mine.
     */
    public int getCount() {
        return -1;
    }
    
    /**
     * Default isFlagged method so that GameBoard can call isFlagged on Space
     * when it determines whether all Mines have been flagged by the user. This
     * method is overridden by Mine.
     */
    public boolean isFlagged() {
        return false;
    }
    
    /**
     * Default isClicked method so that GameBoard can call isClicked on Space
     * when it determines how many Blanks have been clicked by the user. This
     * method is overridden by Blank.
     */
    public boolean isClicked() {
        return false;
    }
    
}
