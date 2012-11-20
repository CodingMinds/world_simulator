package gui;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;

import javax.swing.JComponent;

import client.client;

public class Gui_paint_grid extends JComponent {

	/**
	 * 
	 * paints the view of the discovered world
	 * 
	 * @author andre freudenreich,<andre.freudenreich@gmx.net>
	 */
	private static final long serialVersionUID = 1L;
	private client client = null;

	public Gui_paint_grid(final client client) {
		this.client = client;
		this.setPreferredSize(new Dimension(500, 550));
	}

	// the grid that should be drawn could be much larger
	// then the usable drawing space.
	// so only a view with limited x and x is been drawn.
	// draw a grid around the agent
	// with max x-10 and x+10
	// y-10 and y+10
	// depending the agent x and y coordinates

	private void paintviewgrid(Graphics g) {

		if (client.getgrid_copie() != null) {
			System.out.println(client.getgrid_copie().length);
			int agent_x = client.getAgent_x();
			int agent_y = client.getAgent_y();
			int x_displacment = 0;
			int y_displacment = 0;
			g.setColor(Color.BLACK);

			int agent_displacement_x = agent_x - 10;
			int agent_displacement_y = agent_y - 10;
			for (int y = 0; y < 20; y++) {
				for (int x = 0; x < 20; x++) {
					g.drawRect(x_displacment, y_displacment, 25, 25);
					if (agent_displacement_x >= client.getgrid_x()) {
						continue;
					}
					if (agent_displacement_y >= client.getgrid_y()) {
						continue;
					}
					g.drawString(
							client.getgrid_copie()[agent_displacement_x][agent_displacement_y],
							x_displacment + 10, y_displacment + 20);
					agent_displacement_x++;
					x_displacment = x_displacment + 25;
				}
				x_displacment = 0;
				y_displacment = y_displacment + 25;
				agent_displacement_x = agent_x - 5;
				agent_displacement_y++;
			}
		}
	}

	// draw grid
	private void paintnormalgrid(Graphics g) {
		if (client.getgrid_copie() != null) {
			int x_displacment = 0;
			int y_displacment = 0;
			g.setColor(Color.BLACK);
			for (int y = 0; y < client.getgrid_y(); y++) {
				for (int x = 0; x < client.getgrid_x(); x++) {
					g.drawRect(x_displacment, y_displacment, 25, 25);
					g.drawString(client.getgrid_copie()[x][y],
							x_displacment + 10, y_displacment + 20);
					x_displacment = x_displacment + 25;
				}
				x_displacment = 0;
				y_displacment = y_displacment + 25;
			}
		}
	}

	public void paintComponent(Graphics g) {

		if (client.getgrid_copie() != null) {
			if (client.getgrid_copie().length <= 25)
				paintnormalgrid(g);

			else
				paintviewgrid(g);
		}

	}
}
