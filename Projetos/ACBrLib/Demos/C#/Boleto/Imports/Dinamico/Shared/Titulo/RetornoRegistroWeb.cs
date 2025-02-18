using System;
using System.Collections.Generic;

namespace ACBrLib.Boleto
{
    public sealed class RetornoRegistroWeb
    {
        #region Properties

        public string CodRetorno { get; set; }

        public string OriRetorno { get; set; }

        public string MsgRetorno { get; set; }

        public string Excecao { get; set; }

        public bool IndicadorContinuidade { get; set; }

        public int ProximoIndice { get; set; }

        public string Header_Versao { get; set; }

        public string Header_Autenticacao { get; set; }

        public string Header_Usuario_Servico { get; set; }

        public string Header_Usuario { get; set; }

        public string Header_Operacao { get; set; }

        public int Header_Indice { get; set; }

        public string Header_Sistema_Origem { get; set; }

        public string Header_Agencia { get; set; }

        public string Header_ContaCorrente { get; set; }

        public string Header_Id_Origem { get; set; }

        public DateTime Header_Data_Hora { get; set; }

        public string Header_Id_Processo { get; set; }

        public string Header_CNPJCPF_Beneficiario { get; set; }

        public string ControleOriRetorno { get; set; }

        public string ControleCodRetorno { get; set; }

        public string ControleNSU { get; set; }

        public string ControleRetorno { get; set; }

        public DateTime ControleData { get; set; }

        public string ControleHora { get; set; }

        public string IDCodBarras { get; set; }

        public string IDLinha { get; set; }

        public string IDNossoNum { get; set; }

        public string IDURL { get; set; }

        public List<RetornoRejeicaoWeb> Rejeicoes { get; } = new List<RetornoRejeicaoWeb>();

        public RetornoTituloWeb Titulo { get; } = new RetornoTituloWeb();

        #endregion Properties
    }
}