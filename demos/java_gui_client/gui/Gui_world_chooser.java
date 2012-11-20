package gui;

import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;
import javax.swing.BorderFactory;
import javax.swing.DefaultListModel;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.border.Border;
import client.client;
import java.awt.BorderLayout;

public class Gui_world_chooser extends JPanel {

	/**
	 * list up all worlds
	 * 
	 * 
	 * @author andre freudenreich,<andre.freudenreich@gmx.net>
	 */
	private static final long serialVersionUID = 1L;
	private client client = null;
	private DefaultListModel<String> lmWorld_names = null;
	private DefaultListModel<String> lmWorld_x = null;
	private DefaultListModel<String> lmWorld_y = null;
	private DefaultListModel<String> lmWorld_agent = null;
	private DefaultListModel<String> lmWorld_maxagents = null;
	private Border raisedbevel = BorderFactory.createRaisedBevelBorder();
	private Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	private JLabel string_def = null;
	private ArrayList<String> filtered = null;

	public Gui_world_chooser(final client client) {
		this.client = client;
		this.lmWorld_names = new DefaultListModel<String>();
		this.lmWorld_x = new DefaultListModel<String>();
		this.lmWorld_y = new DefaultListModel<String>();
		this.lmWorld_agent = new DefaultListModel<String>();
		this.lmWorld_maxagents = new DefaultListModel<String>();
		final JList<String> worldlist = new JList<String>(lmWorld_names);
		MouseListener mouseListener = new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				JList<String> tempList = (JList<String>) e.getSource();
				if (tempList.equals(worldlist)) {
					int index = worldlist.locationToIndex(e.getPoint());
					String str = client.getworld_List().get(index + 1);
					// filter " "
					filtered = new ArrayList<String>();
					int tempindex = 0;
					for (int u = 0; u < str.length(); u++) {
						char token = str.charAt(u);
						if (token == ' ') {
							filtered.add(str.substring(tempindex + 1, u));
							tempindex = u;
						}
					}
					client.load_World(filtered.get(1));
				}
			}
		};
		worldlist.addMouseListener(mouseListener);
		JList<String> worldx = new JList<String>(lmWorld_x);
		JList<String> worldy = new JList<String>(lmWorld_y);
		JList<String> worldagent = new JList<String>(lmWorld_agent);
		JList<String> worldmaxagent = new JList<String>(lmWorld_maxagents);
		this.setPreferredSize(new Dimension(220, 550));
		this.setLayout(new BorderLayout());
		this.setBorder(BorderFactory.createCompoundBorder(raisedbevel,
				loweredbevel));
		JPanel content_north = new JPanel(new GridLayout(2, 1));
		content_north.add(new JLabel("List of worlds"));
		this.string_def = new JLabel();
		content_north.add(this.string_def);
		this.add(content_north, BorderLayout.NORTH);
		this.add(worldlist, BorderLayout.WEST);
		JPanel content = new JPanel(new GridLayout(1, 4, 10, 10));
		content.add(worldx);
		content.add(worldy);
		content.add(worldagent);
		content.add(worldmaxagent);
		this.add(content, BorderLayout.CENTER);

	}

	public void clear_List() {
		this.lmWorld_names.clear();
		this.lmWorld_x.clear();
		this.lmWorld_y.clear();
		this.lmWorld_agent.clear();
		this.lmWorld_maxagents.clear();
	}

	public void updateList() {
		clear_List();
		if (client.getworld_List().isEmpty() == true)
			return;
		String string_cut = client.getworld_List().get(0).substring(7);
		this.string_def.setText(string_cut);
		for (int i = 1; i < client.getworld_List().size(); i++) {

			String str = client.getworld_List().get(i);

			// filter " "
			filtered = new ArrayList<String>();
			int tempindex = 0;
			for (int u = 0; u < str.length(); u++) {
				char token = str.charAt(u);
				if (token == ' ') {
					filtered.add(str.substring(tempindex, u));
					tempindex = u;
				}
			}
			if (tempindex < str.length()) {
				filtered.add(str.substring(tempindex, str.length()));
			}
			this.lmWorld_names.addElement(" " + filtered.get(2) + " ");
			this.lmWorld_x.addElement(filtered.get(3));
			this.lmWorld_y.addElement(filtered.get(4));
			this.lmWorld_agent.addElement(filtered.get(5));
			this.lmWorld_maxagents.addElement(filtered.get(6));
		}
	}
}
