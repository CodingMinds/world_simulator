package client;

import gui.Gui_Screen;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Timer;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import utils.Goagent;

/*
 * Simple Java demo that connects to a server via sockets.
 * 
 * the app consists of:
 * A world loader
 * A user controlled agent 
 * An random controlled agent.
 * 
 * description:
 * 
 * a ai/user controlled agent try to find food in a maze.
 * 
 * 
 * Usage:
 * 
 * press "connect" button.
 * choose fitting world.
 * 
 * controlling the (user)agent via keys
 * or
 * press "ai" button for the ai controlled agent. 
 * the connection will be terminated after the food has been found.
 * 
 * 19 November 2012.
 */
/**
 * class client
 * 
 * show Gui,
 * connect to server
 * load world
 * disconnected from server
 * exit program
 * 
 *
 * @author andre freudenreich,<andre.freudenreich@gmx.net>
 */
public class client {
    //config

    String url = "localhost";
    int port =4567;
    private int agentPos_x = 1;
    private int agentPos_y = 1;
    private String grid_original[][] = null;
    private String grid_copie[][] = null;
    public int grid_x = 0;
    public int grid_y = 0;
    private Socket client_socket = null;
    private PrintWriter out = null;
    private BufferedReader in = null;
    private Gui_Screen gui_display = null;
    private Goagent go_agent = null;
    private ArrayList<String> worldlist = new ArrayList<String>();

    public client() {
        this.gui_display = new Gui_Screen(this);
    }

    public int getAgent_x() {
        return this.agentPos_x;
    }

    public int getAgent_y() {
        return this.agentPos_y;
    }

    public ArrayList<String> getworld_List() {
        return this.worldlist;
    }

    public Gui_Screen getgui_display() {
        return this.gui_display;
    }

    public int getgrid_x() {
        return this.grid_x;
    }

    public int getgrid_y() {
        return this.grid_y;
    }

    public String[][] getgrid_copie() {
        return this.grid_copie;
    }

    public BufferedReader getIn_Stream() {
        return this.in;
    }

    public PrintWriter getOut_Stream() {
        return this.out;
    }

    public Socket getclient_socket() {
        return this.client_socket;
    }

    public void setgrid_copie(String[][] grid_copie) {
        this.grid_copie = grid_copie;
    }

    //connect to the world server
    
    public void client_connect() {
        try {
            this.client_socket = new Socket(url, port);
            this.out = new PrintWriter(client_socket.getOutputStream(), true);
            this.in = new BufferedReader(new InputStreamReader(
                    client_socket.getInputStream()));
            String server_status = this.in.readLine();
            //System.out.println("<< " + server_status + "\n");
            this.gui_display.gui_control.setText("<< " + server_status);

            out.println("world list");

            while (true) {
                server_status = this.in.readLine();
                if (server_status.equals("105 EOL")) {
                    break;
                }
                worldlist.add(server_status);
                System.out.println("echo: " + server_status);
            }
        } catch (UnknownHostException e) {
            System.err.println("Don't know host: " + url);
            System.exit(1);
        } catch (IOException e) {
            Logger.getLogger(client.class.getName()).log(Level.SEVERE, null, e);
            System.exit(1);
        }
        gui_display.gui_world_chooser.updateList();
        gui_display.repaint();
    }

    // loads a chosen world
    
    public void load_World(String id) {
        out.println("world load " + id);
        try {
            String server_status = this.in.readLine();
            //System.out.println("<< " + server_status + "\n");
            this.gui_display.gui_control.setText("<< " + server_status);

            if (server_status.contains("403") != true) {
                this.grid_original = create_Grid(server_status);
                out.println("environ");
                String server_environ = in.readLine();
                grid_copie = grid_Manipulator(server_environ);
                gui_display.repaint();
            } else {
                this.gui_display.gui_control.setText("loading aborted");
            }
        } catch (IOException e) {
            Logger.getLogger(client.class.getName()).log(Level.SEVERE, null, e);
        }
    }

    // disconnects form the world
    // closing in and output stream
    
    
    public boolean client_disconnect()  {
        try {
            this.out.println("quit");
            this.gui_display.gui_control.setText("<< " + this.in.readLine());
            this.out.close();
            this.in.close();
            this.client_socket.close();
            this.gui_display.gui_control.setText("connection closed");
            this.worldlist.clear();
            this.grid_copie=null;
            this.gui_display.gui_world_chooser.updateList();
            if (this.go_agent != null) {
                this.go_agent.setstop();
            }
            gui_display.repaint();
            Thread.sleep(200);
        } catch (InterruptedException ex) {
                Logger.getLogger(client.class.getName()).log(Level.SEVERE, null, ex);
        
        } catch (IOException ex) {
            Logger.getLogger(client.class.getName()).log(Level.SEVERE, null, ex);
            return false;
        }
        return true;
    }

    public void client_close() {
        this.gui_display.dispose();
    }

    // user driven agent

    public void mainUseraktion(String client_request) {
        gui_display.gui_control.setText(client_request);
        out.println(client_request);
        try {
            String server_status = in.readLine();
            String text_output = status_verifier(server_status, client_request);
            gui_display.gui_control.setText(text_output);
            out.println("environ");
            String server_environ = in.readLine();
            gui_display.gui_control.setText("<< " + server_environ);
            grid_copie = grid_Manipulator(server_environ);
            gui_display.repaint();
        } catch (IOException ex) {
            Logger.getLogger(client.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
   
    // ai agent 
    // the ai logic is in the the class "timer"
    
    public void mainAutoloop() {
        Timer timer = new Timer();
        go_agent = new Goagent(this);
        timer.schedule(go_agent, 200, 200);
    }

    // creates a grid according to the given x,y values
    // parsed from the received string.
    
    public String[][] create_Grid(String str) {
        String[] string_tokens = str.split(" ");
        String[] int_tokens = string_tokens[4].split("x");

        int y = Integer.parseInt(int_tokens[1]);
        int x = Integer.parseInt(int_tokens[0]);

        this.agentPos_x = 1;
        this.agentPos_y = 1;
        this.agentPos_x = this.agentPos_x * x;
        this.agentPos_y = this.agentPos_y * y;
        this.grid_y = y * 2 + 2;
        this.grid_x = x * 2 + 2;
        String grid[][] = new String[this.grid_x][this.grid_y];
        for (int a = 0; a < this.grid_x; a++) {
            for (int b = 0; b < this.grid_y; b++) {
                grid[a][b] = ".";
            }
        }
        return grid;
    }
    
    //function status_verifier(server_status,user input)
    //decodes status text and change position of agent according to the given move command

    public String status_verifier(String server_status, String client_request) {
        String string_status_code = "";
        String text_output = "";
        string_status_code = server_status.substring(0, 3);
        if ((string_status_code.equals("201")) || (string_status_code.equals("202"))) {
            int int_move_code = client_request.charAt(5) - 48;
            switch (int_move_code) {
                case 1:
                    this.agentPos_y = agentPos_y - 1;
                    break;
                case 2:
                    this.agentPos_y = agentPos_y - 1;
                    this.agentPos_x = agentPos_x + 1;
                    break;
                case 3:
                    this.agentPos_x = agentPos_x + 1;
                    break;
                case 4:
                    this.agentPos_y = agentPos_y + 1;
                    this.agentPos_x = agentPos_x + 1;
                    break;
                case 5:
                    this.agentPos_y = agentPos_y + 1;
                    break;
                case 6:
                    this.agentPos_y = agentPos_y + 1;
                    this.agentPos_x = agentPos_x - 1;
                    break;
                case 7:
                    this.agentPos_x = agentPos_x - 1;
                    break;
                case 8:
                    this.agentPos_y = agentPos_y - 1;
                    this.agentPos_x = agentPos_x - 1;
                    break;
            }
            text_output = "agent successfull moved";
        }
        if (string_status_code.equals("301")) {
            text_output = "agent died";
        } else if (string_status_code.equals("202")) {
            text_output = "agent found food";
        } else if (string_status_code.equals("501")) {
            text_output = "world destroyed";
        } else if (string_status_code.equals("203")) {
            text_output = "path blocked";
        } else if (string_status_code.equals("204")) {
            text_output = "path blocked by another agent";
        }
        return text_output;
    }
    
    // Function manipulator(server_output)
    // place objects and agent position into the matrix
    // objects will be placed around the agent according to his coordinates

    public String[][] grid_Manipulator(String server_str) {
        String cutted_str = "";
        for (int i = 0; i < server_str.length(); i++) {
            if (server_str.charAt(i) == ':') {
                cutted_str = server_str.substring(i + 1, server_str.length());
            }
        }
        Pattern pattern = Pattern.compile("[.OF*]");
        Matcher matcher = pattern.matcher(cutted_str);
        // Check all occurance
        while (matcher.find()) {
          System.out.print(matcher.group());
        }
        
        
        this.grid_original[agentPos_x][agentPos_y - 1] = String.valueOf(cutted_str.charAt(0));
        this.grid_original[agentPos_x + 1][agentPos_y - 1] = String.valueOf(cutted_str.charAt(1));
        this.grid_original[agentPos_x + 1][agentPos_y] = String.valueOf(cutted_str.charAt(2));
        this.grid_original[agentPos_x + 1][agentPos_y + 1] = String.valueOf(cutted_str.charAt(3));
        this.grid_original[agentPos_x][agentPos_y + 1] = String.valueOf(cutted_str.charAt(4));
        this.grid_original[agentPos_x - 1][agentPos_y + 1] = String.valueOf(cutted_str.charAt(5));
        this.grid_original[agentPos_x - 1][agentPos_y] = String.valueOf(cutted_str.charAt(6));
        this.grid_original[agentPos_x - 1][agentPos_y - 1] = String.valueOf(cutted_str.charAt(7));

        String gridcopy[][] = new String[this.grid_x][this.grid_y];
        for (int a = 0; a < this.grid_x; a++) {
            for (int b = 0; b < this.grid_y; b++) {
                gridcopy[a][b] = this.grid_original[a][b];
            }
        }
        gridcopy[agentPos_x][agentPos_y] = "*";
        return gridcopy;
    }

    public static void main(String[] args) throws IOException {

        new client();

    }
}