import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.MouseEvent;

import javax.swing.SwingUtilities;

@SuppressWarnings("serial")
public class Mine extends Space implements Comparable<Mine> {

    private int count = -1;
    
    private static final String mine_file = "images.png";
    private static final String blank_file = "blank.png";
    private static final String flag_file = "flag.png";
    private static Image mine;
    private static Image blank;
    private static Image flag;
    private Image currentImage;
    
    public int POS_X;
    public int POS_Y;

    public Mine(int posX, int posY) {
        this.POS_X = posX;
        this.POS_Y = posY;
        mine = makeImage(mine_file, mine);
        blank = makeImage(blank_file, blank);
        flag = makeImage(flag_file, flag);
        currentImage = blank;
    }
    
    /**
     * Returns the x-position of the Mine.
     */
    public int getX() {
        return POS_X;
    }
    
    /**
     * Returns the y-position of the Mine.
     */
    public int getY() {
        return POS_Y;
    }
    
    @Override
    public boolean isFlagged() {
        return currentImage == flag;
    }
    
    @Override
    public void setCount(int count) {
        this.count = count;
    }
    
    @Override
    public int getCount() {
        return count;
    }
    
    @Override
    public void draw(Graphics g) {
        g.drawImage(currentImage, 0, 0, 20, 20, null);
    }
    
    @Override
    public void handle(MouseEvent e) {
        if (SwingUtilities.isLeftMouseButton(e)) {
            currentImage = mine;
        } else if (SwingUtilities.isRightMouseButton(e)) {
            currentImage = flag;
        }
    }
    
    @Override
    public boolean equals (Object o) {
        if (this == o) {
            return true;
        } else if (o == null) {
            return false;
        } else if (this.getClass() != o.getClass()) {
            return false;
        }
        Mine other = (Mine) o;
        if (this.POS_X != other.POS_X) {
            return false;
        } else if (this.POS_Y != other.POS_Y) {
            return false;
        }
        return true;
    }
    
    public int compareTo(Mine s) {
        if (s.POS_X > this.POS_X) {
                return 1;
        } else if (s.POS_X < this.POS_X) {
            return -1;
        } else if (s.POS_X == this.POS_X) {
            if (s.POS_Y > this.POS_Y) {
                return 1;
            } else if (s.POS_Y < this.POS_Y) {
                return -1;
            } else {
                return 0;
            }
        } else {
            return 0;
        }
    }
    
}
