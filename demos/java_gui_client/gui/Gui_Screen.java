package gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;

import client.client;

public class Gui_Screen extends JFrame {

	/**
	 * 
	 * class gui_screen
	 * 
	 * loads sup gui components and do the layout
	 * 
	 * @author andre freudenreich,<andre.freudenreich@gmx.net>
	 */
	private static final long serialVersionUID = 1L;
	private JComponent paintGrid = null;
	public Gui_control gui_control = null;
	public Gui_world_chooser gui_world_chooser = null;
	private client client = null;

	public Gui_Screen(final client client) {
		this.client = client;
		this.paintGrid = new Gui_paint_grid(this.client);
		this.gui_control = new Gui_control(this.client);
		this.gui_world_chooser = new Gui_world_chooser(this.client);

		this.setPreferredSize(new Dimension(1000, 700));
		this.setTitle("Client");
		this.addWindowListener(new WindowAdapter() {
			public void windowClosing(WindowEvent arg0) {
				client.client_close();
			}
		});
		JPanel content = new JPanel(new BorderLayout());
		content.add(this.gui_world_chooser, BorderLayout.WEST);
		content.add(this.paintGrid, BorderLayout.CENTER);
		content.add(this.gui_control, BorderLayout.SOUTH);

		this.add(content);
		this.pack();
		this.setVisible(true);
	}
}
