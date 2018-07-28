import java.awt.Graphics;
import java.awt.Image;
import java.awt.event.MouseEvent;
import javax.swing.SwingUtilities;

@SuppressWarnings("serial")
public class Blank extends Space implements Comparable<Blank> {

    private int count;
    
    private static final String blank_file = "blank.png";
    private static final String one_file = "one.png";
    private static final String two_file = "two.png";
    private static final String three_file = "three.png";
    private static final String four_file = "four.png";
    private static final String five_file = "five.png";
    private static final String six_file = "six.png";
    private static final String seven_file = "seven.png";
    private static final String eight_file = "eight.png";
    private static final String flag_file = "flag.png";
    private static Image blank;
    private static Image one;
    private static Image two;
    private static Image three;
    private static Image four;
    private static Image five;
    private static Image six;
    private static Image seven;
    private static Image eight;
    private static Image flag;
    private Image currentImage;

    public int POS_X;
    public int POS_Y;

    public Blank(int posX, int posY) {
        this.POS_X = posX;
        this.POS_Y = posY;
        blank = makeImage(blank_file, blank);
        one = makeImage(one_file, one);
        two = makeImage(two_file, two);
        three = makeImage(three_file, three);
        four = makeImage(four_file, four);
        five = makeImage(five_file, five);
        six = makeImage(six_file, six);
        seven = makeImage(seven_file, seven);
        eight = makeImage(eight_file, eight);
        flag = makeImage(flag_file, flag);
        currentImage = blank;
    }
    
    /**
     * Returns the x-position of the Blank.
     */
    public int getX() {
        return POS_X;
    }
    
    /**
     * Returns the y-position of the Blank.
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
    
    private Image getImage(int count) {
        if (count == 1) {
            return one;
        } else if (count == 2) {
            return two;
        } else if (count == 3) {
            return three;
        } else if (count == 4) {
            return four;
        } else if (count == 5) {
            return five;
        } else if (count == 6) {
            return six;
        } else if (count == 7) {
            return seven;
        } else if (count == 8) {
            return eight;
        } else {
            return null;
        }
    }
    
    @Override
    public boolean isClicked() {
        return !(currentImage == blank);
    }
    
    @Override
    public void draw(Graphics g) {
        g.drawImage(currentImage, 0, 0, 20, 20, null);
    }
    
    @Override
    public void handle(MouseEvent e) {
        if (SwingUtilities.isLeftMouseButton(e)) {
            currentImage = getImage(count);
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
        Blank other = (Blank) o;
        if (this.POS_X != other.POS_X) {
            return false;
        } else if (this.POS_Y != other.POS_Y) {
            return false;
        }
        return true;
    }
    
    public int compareTo(Blank s) {
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
