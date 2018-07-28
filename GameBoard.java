import java.awt.Graphics;
import java.awt.event.MouseEvent;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JPanel;
import javax.swing.SwingUtilities;

@SuppressWarnings("serial")
public class GameBoard extends JPanel{
    
    private Space[][] board;
    private int boardWidth;
    private int boardHeight;

    /** Constructs the Game Board. The board is a 2D array of Space objects,
     *  which are chosen at random as they are filled in to dynamically be either
     *  Mine objects or Blank objects. Iterates over each Space in the GameBoard
     *  to determine the number of its neighbors that are Mines and sets the count
     *  for each.
     */
    public GameBoard(int width, int height) {
        this.boardWidth = (int) width/20;
        this.boardHeight = (int) height/20;
        this.board = new Space[boardWidth][boardHeight];
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                if (Math.random() < 0.2) {
                    board[i][j] = new Mine(i, j);
                } else {
                    board[i][j] = new Blank(i, j);
                }
            }
        }
        initializeCounts();
    }
    
    /**
     * Reveals all of the Mines on the GameBoard when a user clicks a mine.
     * 
     * @param e MouseEvent on a Mine
     */
    public void revealMines(MouseEvent e) {
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                if (board[i][j] instanceof Mine) {
                    board[i][j].handle(e);
                }
            }
        }
    }
    
    public boolean isSwept1(MouseEvent e) {
        board[(int) (e.getX() / 20)][(int) (e.getY() / 20)].handle(e);
        boolean result = true;
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                if (board[i][j] instanceof Mine) {
                    result = result && board[i][j].isFlagged();
                }
            }
        }
        return result;
    }
    
    public boolean isSwept2() {
        boolean result = true;
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                if (board[i][j] instanceof Mine) {
                    result = result && board[i][j].isFlagged();
                }
            }
        }
        return result;
    }
    
    public int getSwept() {
        int count = 0;
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                if (board[i][j] instanceof Mine && board[i][j].isFlagged()) {
                    count++;
                } else if (board[i][j] instanceof Blank && board[i][j].isClicked()) {
                    count++;
                }
            }
        }
        return (int) count / 9;
    }
    
    /**
     * getType returns whether a particular cell on the GameBoard is a Mine or a
     * Blank so that the GameCourt can determine whether the game ends.
     * 
     * @param x
     * @param y
     * @return a string to determine which sub-type of Space is at the location
     */
    public String getType(int x, int y) {
        if (board[x][y] instanceof Mine) {
            return "Mine";
        } else {
            return "Blank";
        }
    }
    
    /**
     * Draw traverses the 2D array and calls the draw methods of each of the 
     * Spaces contained, which dynamically dispatch to the draw methods of the
     * Mine or Blank, as appropriate.
     * 
     * @param g 
     *  The <code>Graphics</code> context used for drawing the object.
     *  Gives the context in which the object should be drawn (a canvas,
     *  a frame, etc.)
     */
    public void draw(Graphics g) {
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                board[i][j].draw(g);
                g.translate(0, 20);
            }
            g.translate(20, -boardHeight * 20);
        }
    }
    
    /**
     * Handle passes the MouseEvent on to the appropriate Space based on the position
     * of the event, and it is dynamically dispatched to call either the implementation
     * of Blank or Mine, as appropriate.
     * 
     * @param e
     *  The MouseEvent that triggers the method call.
     */
    public void handle(MouseEvent e) {
        board[(int) (e.getX() / 20)][(int) (e.getY() / 20)].handle(e);
        Set<Space> s = new TreeSet<Space>();
        if (board[(int) (e.getX() / 20)][(int) (e.getY() / 20)].getCount() == 0 && 
                SwingUtilities.isLeftMouseButton(e)) {
            s.add(board[(int) (e.getX() / 20)][(int) (e.getY() / 20)]);
            cascade(s, e);
        }
    }
    
    /**
     * Cascade recursively computes the Spaces that should be revealed as a result of a
     * click when there are more than one adjacent Spaces with zero mine neighbors nearby.
     * It iterates over the set it assembles and calls Handle on each, as if they were
     * clicked as well.
     * 
     * @param Set<Space> s
     * @param MouseEvent e
     */
    public void cascade(Set<Space> s, MouseEvent e) {
        Set<Space> a = new TreeSet<Space>();
        a.addAll(s);
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                if (board[i][j].getCount() == 0 && allNeighbors(board[i][j], a)) {
                    a.add(board[i][j]);
                }
            }
        }
        if (!(a.equals(s))) {
            cascade(a, e);
        }
        Set<Space> n = new TreeSet<Space>();
        n.addAll(a);
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                if (board[i][j].getCount() != -1 && allNeighbors(board[i][j], a)) {
                    n.add(board[i][j]);
                }
            }
        }
        for (Space space : n) {
            space.handle(e);
        }
    }
    
    /**
     * allNeighbors determines if a given Space is in the square around a Space in the
     * Set provided.
     * 
     * @param Space space
     * @param Set<Space> set
     */
    private boolean allNeighbors(Space space, Set<Space> s) {
        boolean result = false;
        for (Space a : s) {
            if (Math.abs(space.getX() - a.getX()) < 2 && Math.abs(space.getY() - a.getY()) < 2) {
                result = true;
            }
        }
        return result;
    }
    
    /**
     * initializeCounts traverses the board and sets the count of each space on the board to
     * the number of mines surrounding each space. Mine cells are set to -1 by default.
     */
    private void initializeCounts() {
        for (int i = 0; i < boardWidth; i++) {
            for (int j = 0; j < boardHeight; j++) {
                int count = 0;
                if (i + 1 < boardWidth) {
                    if (board[i + 1][j] instanceof Mine) {
                        count++;
                    }
                }
                if (i + 1 < boardWidth && j + 1 < boardHeight) {
                    if (board[i + 1][j + 1] instanceof Mine) {
                        count++;
                    }
                }
                if (i + 1 < boardWidth && j - 1 > -1) {
                    if (board[i + 1][j -1] instanceof Mine) {
                        count++;
                    }
                }
                if (j + 1 < boardHeight) {
                    if (board[i][j + 1] instanceof Mine) {
                        count++;
                    }
                }
                if (j + 1 < boardHeight && i - 1 > -1) {
                    if (board[i - 1][j + 1] instanceof Mine) {
                        count++;
                    }
                }
                if (i - 1 > -1) {
                    if (board[i - 1][j] instanceof Mine) {
                        count++;
                    }
                }
                if (i - 1 > -1 && j - 1 > -1) {
                    if (board[i - 1][j - 1] instanceof Mine) {
                        count++;
                    }
                }
                if (j - 1 > -1) {
                    if (board[i][j - 1] instanceof Mine) {
                        count++;
                    }
                }
                board[i][j].setCount(count);
                if (board[i][j] instanceof Mine) {
                    board[i][j].setCount(-1);
                }
            }
        }
    }
    
}
