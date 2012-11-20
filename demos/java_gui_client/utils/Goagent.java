package utils;

import java.io.IOException;
import java.util.Random;
import java.util.TimerTask;
import client.client;

/**
 * ai agent
 * 
 * a timer is used to synchronized agents behavior every time the timer is
 * executed the ai agent does his work.
 * 
 * @author andre freudenreich,<andre.freudenreich@gmx.net>
 */
public class Goagent extends TimerTask {

	private boolean stop = false;
	private Random ran = new Random();
	private int i = 0;
	private client client = null;

	public Goagent(final client client) {
		this.client = client;
	}

	// while the timer is not canceled (this.stop != true)
	// the agent do his work
	// the agent terminates his search until he finds the food

	public void run() {
		if (this.stop != true) {
			try {
				String client_request;
				String server_status;
				String text_output;

				// random value
				int zahl = ran.nextInt(8) + 1;
				client_request = "move " + zahl;
				//
				client.getgui_display().gui_control.setText(i + ": >>"
						+ client_request);
				client.getOut_Stream().println(client_request);

				// server status return
				server_status = client.getIn_Stream().readLine();
				text_output = client.status_verifier(server_status,
						client_request);
				client.getgui_display().gui_control
						.setText("<< " + text_output);
				if (server_status.equals("200 good bye")
						|| server_status.contains("202 food")) {
					setstop();
				}

				// environ request
				client.getOut_Stream().println("environ");
				String server_environ = client.getIn_Stream().readLine();
				client.getgui_display().gui_control.setText("<< "+
						server_environ);
				client.setgrid_copie(client.grid_Manipulator(server_environ));
				client.getgui_display().repaint();
				i++;
			} catch (IOException ex) {
				setstop();
			}
		} else {
			killtimer();
		}
	}

	public void setstop() {
		this.stop = true;
	}

	private void killtimer() {
		client.client_disconnect();
		client.getgui_display().gui_control.kiexit();
		this.cancel();
	}
}
