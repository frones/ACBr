using System;
using System.Drawing.Printing;
using System.IO.Ports;
using System.Windows.Forms;
using ACBrLib.Core.Serial;

namespace ACBrLibPosPrinter.Demo
{
    public partial class FrmSerial : Form
    {
        #region Fields

        public ACBrDevice Device { get; set; }

        #endregion Fields

        #region Constructor

        public FrmSerial()
        {
            InitializeComponent();
        }

        private void SerialCFGForm_Load(object sender, EventArgs e)
        {
            inicializar();
        }

        #endregion Constructor

        #region Methods

        private void inicializar()
        {
            cmbPorta.Items.AddRange(SerialPort.GetPortNames());

            cmbPorta.Items.Add("LPT1");
            cmbPorta.Items.Add("LPT2");
            cmbPorta.Items.Add(@"\\localhost\Epson");
            cmbPorta.Items.Add(@"c:\temp\ecf.txt");
            cmbPorta.Items.Add("TCP:192.168.0.31:9100");

            foreach (string printer in PrinterSettings.InstalledPrinters)
            {
                cmbPorta.Items.Add($"RAW:{printer}");
            }

            cmbPorta.SelectedItem = Device.Porta;

            cmbVelocidade.Items.Add(1200);
            cmbVelocidade.Items.Add(2400);
            cmbVelocidade.Items.Add(4800);
            cmbVelocidade.Items.Add(9600);
            cmbVelocidade.Items.Add(19200);
            cmbVelocidade.Items.Add(38400);
            cmbVelocidade.Items.Add(57600);
            cmbVelocidade.Items.Add(115200);

            cmbVelocidade.SelectedItem = Device.Baud;

            cmbDatabits.Items.Add(5);
            cmbDatabits.Items.Add(6);
            cmbDatabits.Items.Add(7);
            cmbDatabits.Items.Add(8);

            cmbDatabits.SelectedItem = Device.DataBits;

            foreach (var stopbits in Enum.GetValues(typeof(SerialStopBytes))) cmbStopbits.Items.Add(stopbits);
            cmbStopbits.SelectedItem = Device.StopBits;

            foreach (var serialparity in Enum.GetValues(typeof(SerialParity))) cmbParity.Items.Add(serialparity);
            cmbParity.SelectedItem = Device.Parity;

            foreach (var serialhandshake in Enum.GetValues(typeof(SerialHandShake))) cmbHandshaking.Items.Add(serialhandshake);
            cmbHandshaking.SelectedItem = Device.HandShake;

            timeOutNumericUpDown.Value = Device.TimeOut;
            chkHardFlow.Checked = Device.HardFlow;
            chkSoftFlow.Checked = Device.SoftFlow;
        }

        #endregion Methods

        #region EventHandler

        private void button1_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            this.Close();
        }

        private void cmbPorta_SelectedIndexChanged(object sender, EventArgs e)
        {
            Device.Porta = (string)cmbPorta.SelectedItem;
        }

        private void cmbVelocidade_SelectedIndexChanged(object sender, EventArgs e)
        {
            Device.Baud = (int)cmbVelocidade.SelectedItem;
        }

        private void timeOutNumericUpDown_ValueChanged(object sender, EventArgs e)
        {
            Device.TimeOut = (int)timeOutNumericUpDown.Value;
        }

        private void cmbDatabits_SelectedIndexChanged(object sender, EventArgs e)
        {
            Device.DataBits = (int)cmbDatabits.SelectedItem;
        }

        private void cmbStopbits_SelectedIndexChanged(object sender, EventArgs e)
        {
            Device.StopBits = (SerialStopBytes)cmbStopbits.SelectedItem;
        }

        private void cmbParity_SelectedIndexChanged(object sender, EventArgs e)
        {
            Device.Parity = (SerialParity)cmbParity.SelectedItem;
        }

        private void cmbHandshaking_SelectedIndexChanged(object sender, EventArgs e)
        {
            Device.HandShake = (SerialHandShake)cmbHandshaking.SelectedItem;
        }

        private void checkBox1_CheckedChanged(object sender, EventArgs e)
        {
            Device.HardFlow = chkHardFlow.Checked;
        }

        private void checkBox2_CheckedChanged(object sender, EventArgs e)
        {
            Device.SoftFlow = chkSoftFlow.Checked;
        }

        #endregion EventHandler
    }
}