using System;

namespace ACBrLib.Sat
{
    public sealed class RetornoStatusSat
    {
        public string NSERIE { get; set; }

        public string LAN_MAC { get; set; }

        public string STATUS_LAN { get; set; }

        public string NIVEL_BATERIA { get; set; }

        public string MT_TOTAL { get; set; }

        public string MT_USADA { get; set; }

        public DateTime DH_ATUAL { get; set; }

        public string VER_SB { get; set; }

        public string VER_LAYOUT { get; set; }

        public string ULTIMO_CFe { get; set; }

        public string LISTA_INICIAL { get; set; }

        public string LISTA_FINAL { get; set; }

        public DateTime DH_CFe { get; set; }

        public DateTime DH_ULTIMA { get; set; }

        public DateTime CERT_EMISSAO { get; set; }

        public DateTime CERT_VENCIMENTO { get; set; }

        public string ESTADO_OPERACAO { get; set; }
    }
}