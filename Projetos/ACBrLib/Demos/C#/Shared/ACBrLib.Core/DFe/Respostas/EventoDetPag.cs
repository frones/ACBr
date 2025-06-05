using System;

namespace ACBrLib.Core.DFe.Respostas
{
    public sealed class EventoDetPag
    {
        #region Properties

        public string CNPJPag { get; set; }
        public string CNPJReceb { get; set; }
        public string UFPag { get; set; }
        public string UFReceb { get; set; }
        public string cAut { get; set; }
        public DateTime dPag { get; set; }
        public int indPag { get; set; }
        public string tBand { get; set; }
        public string tPag { get; set; }
        public double vPag { get; set; }

        #endregion Properties
    }
}
