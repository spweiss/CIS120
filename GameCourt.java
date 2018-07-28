/**
 * CIS 120 Game HW
 * (c) University of Pennsylvania
 * @version 2.0, Mar 2013
 */

import java.awt.*;
import java.awt.event.*;
import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Iterator;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import javax.swing.*;


/**
 * GameCourt
 * 
 * This class holds the primary game logic for how different objects interact
 * with one another. Take time to understand how the timer interacts with the
 * different methods and how it repaints the GUI on every tick().
 * 
 */
@SuppressWarnings("serial")
public class GameCourt extends JPanel {
    
    private GameBoard board;

	public boolean playing = false; // whether the game is running
	private JLabel status; // Current status text (i.e. Running...)

	public static final int COURT_WIDTH = 600;
	public static final int COURT_HEIGHT = 600;
	public static final int INTERVAL = 35;

	public GameCourt(final JLabel status) {

		setFocusable(true);
		
		final Timer timer = new Timer(INTERVAL, new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                tick();
            }
        });
        timer.start();
		
		addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (board.getType((int) (e.getX() / 20), (int) (e.getY() / 20)).equals("Mine")) {
                    if (SwingUtilities.isLeftMouseButton(e)) {
                        board.revealMines(e);
                        status.setText("Game Over!");
                        playing = false;
                        highScoreEntry();
                    } else if (SwingUtilities.isRightMouseButton(e)) {
                        if (board.isSwept1(e)) {
                            status.setText("You Win!");
                            playing = false;
                        }
                    }
                }
                board.handle(e);
            }
        });

		this.status = status;
	}

	/**
	 * (Re-)set the game to its initial state.
	 */
	public void reset() {
	    board = new GameBoard(COURT_WIDTH, COURT_HEIGHT);

		playing = true;
		status.setText("Board Swept: " + board.getSwept() + "%");

		// Make sure that this component has the keyboard focus
		requestFocusInWindow();
		
		repaint();
	}
	
	/**
     * Opens the instructions window.
     */
    public void instructions() {
        
        final JFrame help = new JFrame("Instructions");
        help.setLocation(300, 300);

        final JPanel help_panel = new JPanel();
        help.add(help_panel);
        final JLabel text = new JLabel("<html>Welcome to Minesweeper! "
                + "Now you don't need to dual-boot Windows 95!<br><br>The goal of the game is to "
                + "clear the board of mines without tripping any.<br>Click the cells on the board "
                + "to reveal them. The numbers signify the number<br>of mines in the surrounding "
                + "eight cells. If you believe that there is a mine<br>in a particular cell, "
                + "right-click it to flag it as a mine. If you flag all of the<br>mines on the "
                + "board without tripping any, then you win!<br><br>Good luck, Corporal! And watch "
                + "out for them pesky mines!</html>");
        help_panel.add(text);
        
        help.pack();
        help.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        help.setVisible(true);
    }
    
    /**
     * Opens the high scores window.
     */
    public void highScoreView() {
        String result = "";
        try (Scanner in = new Scanner(new FileReader("highscores.txt"))) {
            Map<String, Integer> scores = new TreeMap<String, Integer>();
            while (in.hasNextLine()) {
                in.useDelimiter(" ");
                String firstname = in.next();
                in.useDelimiter("%");
                Integer score = Integer.parseInt(in.next().substring(1));
                in.useDelimiter("\n");
                in.next();
                scores.put(firstname, score);
            }
            Set<Map.Entry<String, Integer>> scoreSet = new TreeSet<Map.Entry<String, Integer>>();
            scoreSet = scores.entrySet();
            StringBuffer scoreString = new StringBuffer();
            for (int i = 1; i < 11; i++) {
                Integer max = -1;
                String maxperson = "";
                for (Map.Entry<String, Integer> x : scoreSet) {
                    if (x.getValue() > max) {
                        max = x.getValue();
                        maxperson = x.getKey();
                    }
                }
                Iterator<Map.Entry<String, Integer>> iter = scoreSet.iterator();
                while (iter.hasNext()) {
                    if (iter.next().getKey().equals(maxperson)) {
                        iter.remove();
                    }
                }
                scoreString.append("<br>" + i + ". " + maxperson + " " + max + "%<br>");
            }
            result = scoreString.toString();
        } catch (FileNotFoundException x){
            final JFrame error = new JFrame("Error");
            error.setLocation(300, 300);

            final JPanel error_panel = new JPanel();
            error.add(error_panel);
            final JLabel text = new JLabel("Internal File Not Found");
            error_panel.add(text);
            
            error.pack();
            error.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            error.setVisible(true);
        }
        
        final JFrame scores = new JFrame("High Scores");
        scores.setLocation(300, 300);

        final JPanel scores_panel = new JPanel();
        scores.add(scores_panel);
        final JLabel text = new JLabel("<html>Minesweeper High Scores:                         "
                + "<br>" + result + "</html>");
        scores_panel.add(text);
        
        scores.pack();
        scores.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        scores.setVisible(true);
    }
    
    public void highScoreEntry() {
        final JFrame entry = new JFrame("Congratulations!");
        entry.setLocation(300, 300);

        final JPanel entry_panel = new JPanel();
        entry.add(entry_panel);
        final JTextField text = new JTextField("Enter your first name (no spaces)");
        entry_panel.add(text);
        
        final JButton enter = new JButton("Enter");
        enter.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                String name = text.getText();
                if (name.contains(" ")) {
                    final JFrame error = new JFrame("Error");
                    error.setLocation(300, 300);

                    final JPanel error_panel = new JPanel();
                    error.add(error_panel);
                    final JLabel text = new JLabel("Invalid name format! Try again.");
                    error_panel.add(text);
                    
                    error.pack();
                    error.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
                    error.setVisible(true);
                } else {
                    try (FileWriter fw = new FileWriter("highscores.txt", true);
                            BufferedWriter bw = new BufferedWriter(fw);
                            PrintWriter out = new PrintWriter(bw)) {
                        StringWriter buf = new StringWriter();
                        buf.write("\n" + name + " " + board.getSwept() + "%");
                        out.print(buf.toString());
                        entry.dispose();
                        reset();
                    } catch (FileNotFoundException e1) {
                        final JFrame error = new JFrame("Error");
                        error.setLocation(300, 300);

                        final JPanel error_panel = new JPanel();
                        error.add(error_panel);
                        final JLabel text = new JLabel("Internal File Not Found");
                        error_panel.add(text);
                        
                        error.pack();
                        error.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
                        error.setVisible(true);
                    } catch (IOException e1) {
                        final JFrame error = new JFrame("Error");
                        error.setLocation(300, 300);

                        final JPanel error_panel = new JPanel();
                        error.add(error_panel);
                        final JLabel text = new JLabel("Internal File Error");
                        error_panel.add(text);
                        
                        error.pack();
                        error.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
                        error.setVisible(true);
                    }
                }
            }
        });
        entry_panel.add(enter);
        
        entry.pack();
        entry.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
        entry.setVisible(true);
    }
    
    public int getSwept() {
        return board.getSwept();
    }

	/**
	 * This method is called every time the timer defined in the constructor
	 * triggers.
	 */
	void tick() {
	    repaint();
		if (playing) {
			status.setText("Board Swept: " + board.getSwept() + "%");
		}
	}

	@Override
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		board.draw(g);
	}

	@Override
	public Dimension getPreferredSize() {
		return new Dimension(COURT_WIDTH, COURT_HEIGHT);
	}
}
