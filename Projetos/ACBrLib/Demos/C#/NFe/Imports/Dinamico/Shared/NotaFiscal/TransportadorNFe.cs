using System.Collections.Generic;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Informações do Transporte da NF-e
    /// </summary>
    public class TransportadorNFe
    {
        #region Properties

        /// <summary>
        /// Modalidade do frete
        /// <para>0=Por conta do emitente;</para>
        /// <para>1=Por conta do destinatário/remetente;</para>
        /// <para>2=Por conta de terceiros;</para>
        /// <para>9=Sem frete;</para>
        /// </summary>
        public ModalidadeFrete modFrete { get; set; }

        /// <summary>
        /// CNPJ ou CPF do Transportador
        /// </summary>
        public string CNPJCPF { get; set; }

        /// <summary>
        /// Razão Social ou nome
        /// </summary>
        public string xNome { get; set; }

        /// <summary>
        /// Inscrição Estadual do Transportador.
        /// Informar:
        /// <para>- Inscrição Estadual do transportador contribuinte do ICMS, sem caracteres de formatação(ponto, barra, hífen, etc.);</para>
        /// <para>- Literal "ISENTO" para transportador isento de inscrição no cadastro de contribuintes ICMS;</para>
        /// <para>- Não informar a tag para não contribuinte do ICMS</para>
        /// <para>- A UF deve ser informada se informado uma IE.</para>
        /// </summary>
        public string IE { get; set; }

        /// <summary>
        /// Endereço Completo
        /// </summary>
        public string xEnder { get; set; }

        /// <summary>
        /// Nome do município
        /// </summary>
        public string xMun { get; set; }

        /// <summary>
        /// Sigla da UF. Informar "EX" para Exterior.
        /// </summary>
        public string UF { get; set; }

        /// <summary>
        /// Valor do Serviço
        /// </summary>
        public decimal? vServ { get; set; }

        /// <summary>
        /// BC da Retenção do ICMS
        /// </summary>
        public decimal? vBCRet { get; set; }

        /// <summary>
        /// Alíquota da Retenção
        /// </summary>
        public decimal? pICMSRet { get; set; }

        /// <summary>
        /// Valor do ICMS Retido
        /// </summary>
        public decimal? vICMSRet { get; set; }

        /// <summary>
        /// CFOP de Serviço de Transporte
        /// </summary>
        public string CFOP { get; set; }

        /// <summary>
        /// Código do município de ocorrência do fato gerador do ICMS do transporte. Utilizar a Tabela do IBGE.
        /// </summary>
        public int cMunFG { get; set; }

        /// <summary>
        /// Placa do Veículo
        /// <para>Informar em um dos seguintes formatos: XXX9999, XXX999, XX9999 ou XXXX999.</para>
        /// <para>Informar a placa em informações complementares quando a placa do veículo tiver lei de formação diversa.</para>
        /// </summary>
        public string Placa { get; set; }

        /// <summary>
        /// Sigla da UF. Informar "EX" para Exterior.
        /// </summary>
        public string UFPlaca { get; set; }

        /// <summary>
        /// Registro Nacional de Transportador de Carga(ANTT)
        /// </summary>
        public string RNTC { get; set; }

        /// <summary>
        /// Identificação do vagão
        /// </summary>
        public string vagao { get; set; }

        /// <summary>
        /// Identificação da balsa
        /// </summary>
        public string balsa { get; set; }

        public List<ReboqueNFe> Reboque { get; } = new List<ReboqueNFe>();

        #endregion Properties
    }
}