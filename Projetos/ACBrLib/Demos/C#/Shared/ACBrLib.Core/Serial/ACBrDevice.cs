using System;
using System.Text;

namespace ACBrLib.Core.Serial
{
    public sealed class ACBrDevice
    {
        #region Constructors

        public ACBrDevice()
        {
            Porta = string.Empty;
            Baud = 9600;
            DataBits = 8;
            Parity = SerialParity.None;
            StopBits = SerialStopBytes.One;
            HandShake = SerialHandShake.Nenhum;
            HardFlow = false;
            SoftFlow = false;
            MaxBandwidth = 0;
            SendBytesInterval = 0;
            SendBytesCount = 0;
        }

        public ACBrDevice(string deviceParams) : this()
        {
            if (string.IsNullOrEmpty(deviceParams)) return;
            if (string.IsNullOrWhiteSpace(deviceParams)) return;

            LerDeviceParams(deviceParams);
        }

        #endregion Constructors

        #region Properties

        public string Porta { get; set; }

        public int Baud { get; set; }

        public int DataBits { get; set; }

        public SerialParity Parity { get; set; }

        public SerialStopBytes StopBits { get; set; }

        public SerialHandShake HandShake { get; set; }

        public bool HardFlow { get; set; }

        public bool SoftFlow { get; set; }

        public int TimeOut { get; set; }

        public int MaxBandwidth { get; set; }

        public int SendBytesCount { get; set; }

        public int SendBytesInterval { get; set; }

        #endregion Properties

        #region Methods

        private void LerDeviceParams(string deviceParams)
        {
            var dParams = deviceParams.Split(' ');
            foreach (var dParam in dParams)
            {
                var parameters = dParam.ToUpper().Split('=');

                switch (parameters[0])
                {
                    case "BAUD":
                        Baud = Convert.ToInt32(parameters[1]);
                        break;

                    case "DATA":
                        DataBits = Convert.ToInt32(parameters[1]);
                        break;

                    case "PARITY":
                        Parity = (SerialParity)Enum.ToObject(typeof(SerialParity), parameters[1][0]);
                        break;

                    case "STOP":
                        StopBits = parameters[1].Trim() == "1" ? SerialStopBytes.One :
                                   parameters[1].Trim() == "1,5" ? SerialStopBytes.OnePointFive : SerialStopBytes.Two;
                        break;

                    case "HARDFLOW":
                        HardFlow = true;
                        break;

                    case "SOFTFLOW":
                        SoftFlow = true;
                        break;

                    case "HANDSHAKE":
                        HandShake = parameters[1].Trim() == "XON/XOFF" ? SerialHandShake.XON_XOFF :
                                   parameters[1].Trim() == "DTR/DSR" ? SerialHandShake.DTR_DSR :
                                   parameters[1].Trim() == "RTS/CTS" ? SerialHandShake.RTS_CTS : SerialHandShake.Nenhum;
                        break;

                    case "MAXBANDWIDTH":
                        MaxBandwidth = Convert.ToInt32(parameters[1]);
                        break;

                    case "SENDBYTESCOUNT":
                        SendBytesCount = Convert.ToInt32(parameters[1]);
                        break;

                    case "SENDBYTESINTERVAL":
                        SendBytesInterval = Convert.ToInt32(parameters[1]);
                        break;

                    default:
                        break;
                }
            }
        }

        public override string ToString()
        {
            var parmsString = new StringBuilder();

            parmsString.Append($"BAUD={Baud} ");
            parmsString.Append($"DATA={DataBits} ");
            parmsString.Append($"PARITY={(char)Parity} ");

            parmsString.Append($@"STOP={(StopBits == SerialStopBytes.One ? "1" :
                                         StopBits == SerialStopBytes.OnePointFive ? "1,5" : "2")} ");

            parmsString.Append($@"HANDSHAKE={(HandShake == SerialHandShake.XON_XOFF ? "XON/XOFF" :
                                              HandShake == SerialHandShake.DTR_DSR ? "DTR/DSR" :
                                              HandShake == SerialHandShake.RTS_CTS ? "RTS/CTS" : "")} ");

            if (HardFlow) parmsString.Append("HARDFLOW ");
            if (SoftFlow) parmsString.Append("SOFTFLOW ");

            parmsString.Append($"MAXBANDWIDTH={MaxBandwidth} ");
            parmsString.Append($"SENDBYTESCOUNT={SendBytesCount} ");
            parmsString.Append($"SENDBYTESINTERVAL={SendBytesInterval} ");

            return parmsString.ToString();
        }

        public static implicit operator ACBrDevice(string deviceParams)
        {
            return new ACBrDevice(deviceParams);
        }

        #endregion Methods
    }
}