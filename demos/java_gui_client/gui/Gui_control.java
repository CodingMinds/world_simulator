package gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.border.Border;

import client.client;

public class Gui_control extends JPanel {

	/**
	 * user can interact with the environment.
	 * 
	 * connect, disconnect, Arrow keys for moving agent, text output
	 * 
	 * @author andre freudenreich,<andre.freudenreich@gmx.net>
	 */
	private static final long serialVersionUID = 1L;
	private JTextArea textfield = null;
	private JButton connect = null;
	private JButton disconnect = null;
	private JButton button_1 = null;
	private JButton button_2 = null;
	private JButton button_3 = null;
	private JButton button_4 = null;
	private JButton button_5 = null;
	private JButton button_6 = null;
	private JButton button_7 = null;
	private JButton button_8 = null;
	private JButton buttonAI = null;
	private Border raisedbevel = BorderFactory.createRaisedBevelBorder();
	private Border loweredbevel = BorderFactory.createLoweredBevelBorder();
	private client client = null;

	public Gui_control(final client client) {
		this.client = client;
		this.setPreferredSize(new Dimension(500, 120));
		this.setBorder(BorderFactory.createCompoundBorder(raisedbevel,
				loweredbevel));
		this.textfield = this.control_TextArea();
		this.add(connection_Panel());
		this.add(this.control_Panel());
		this.add(new JScrollPane(this.textfield));
	}

	public void setText(String str) {
		this.textfield.append(str + "\n");
		this.textfield.setCaretPosition(this.textfield.getDocument()
				.getLength());
	}

	public void kiexit() {
		this.disconnect.setEnabled(false);
		this.connect.setEnabled(true);
	}

	// connect disconnect buttons

	public JPanel connection_Panel() {
		JPanel conPanel = new JPanel(new GridLayout(2, 1));
		this.connect = new JButton("connect");
		this.connect.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.client_connect();
				disconnect.setEnabled(true);
				connect.setEnabled(false);
			}
		});
		this.disconnect = new JButton("disconnect");
		this.disconnect.setEnabled(false);
		this.disconnect.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				boolean performed = client.client_disconnect();
				if (performed == true) {
					connect.setEnabled(true);
					disconnect.setEnabled(false);
				}
			}
		});
		conPanel.add(this.connect);
		conPanel.add(this.disconnect);
		return conPanel;
	}

	// textoutput

	public JTextArea control_TextArea() {

		JTextArea textfield = new JTextArea(5, 30);
		textfield.setBorder(BorderFactory.createCompoundBorder(raisedbevel,
				loweredbevel));
		return textfield;
	}

	// layout the different parts

	public JPanel control_Panel() {
		JPanel panel = new JPanel(new BorderLayout());
		panel.setBorder(BorderFactory.createCompoundBorder(raisedbevel,
				loweredbevel));
		JPanel control = new JPanel(new BorderLayout());
		this.button_8 = new JButton("8");
		this.button_8.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 8");
			}
		});
		this.button_7 = new JButton("7");
		this.button_7.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 7");
			}
		});
		this.button_6 = new JButton("6");
		this.button_6.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 6");
			}
		});
		this.button_5 = new JButton("5");
		this.button_5.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 5");
			}
		});
		this.button_4 = new JButton("4");
		this.button_4.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 4");
			}
		});
		this.button_3 = new JButton("3");
		this.button_3.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 3");
			}
		});
		this.button_2 = new JButton("2");
		this.button_2.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 2");
			}
		});
		this.button_1 = new JButton("1");
		this.button_1.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainUseraktion("move 1");
			}
		});
		this.buttonAI = new JButton("AI");
		this.buttonAI.addActionListener(new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				client.mainAutoloop();
			}
		});

		JPanel north = new JPanel();
		north.add(this.button_8);
		north.add(this.button_1);
		north.add(this.button_2);
		JPanel south = new JPanel();
		south.add(this.button_6);
		south.add(this.button_5);
		south.add(this.button_4);

		control.add(north, BorderLayout.NORTH);
		control.add(this.button_7, BorderLayout.WEST);
		control.add(this.buttonAI, BorderLayout.CENTER);
		control.add(south, BorderLayout.SOUTH);
		control.add(this.button_3, BorderLayout.EAST);
		panel.add(control);

		return panel;
	}
}
