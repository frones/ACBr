using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Identificação do Destinatário da Nota Fiscal eletrônica
    /// </summary>
    public class DestinatarioNFe
    {
        #region Properties

        /// <summary>
        /// Identificação do destinatário no caso de comprador estrangeiro
        /// <para>No caso de operação com o
        /// exterior, ou para comprador estrangeiro informar a tag
        /// "idEstrangeiro", com o número do passaporte ou outro
        /// documento legal para identificar pessoa estrangeira
        /// (campo aceita valor nulo).</para>
        /// </summary>
        public string idEstrangeiro { get; set; }

        /// <summary>
        /// CNPJ ou CPF do destinatário
        /// </summary>
        public string CNPJCPF { get; set; }

        /// <summary>
        /// Razão Social ou Nome do destinatário
        /// </summary>
        public string xNome { get; set; }

        /// <summary>
        /// Indicador da IE do Destinatário
        /// <para>1=Contribuinte ICMS (informar a IE do destinatário);</para>
        /// <para>2=Contribuinte isento de Inscrição no cadastro de Contribuintes do ICMS;</para>
        /// <para>9=Não Contribuinte, que pode ou não possuir Inscrição Estadual no Cadastro de Contribuintes do ICMS.</para>
        /// <para>Nota 1: No caso de NFC-e informar indIEDest=9 e não informar a tag IE do destinatário;</para>
        /// <para>Nota 2: No caso de operação com o Exterior informar indIEDest=9 e não informar a tag IE do destinatário;</para>
        /// <para>Nota 3: No caso de Contribuinte Isento de Inscrição (indIEDest= 2), não informar a tag IE do destinatário.</para>
        /// </summary>
        public IndicadorIE indIEDest { get; set; }

        /// <summary>
        /// Inscrição Estadual do destinatário
        /// <para>Campo opcional. Informar somente os algarismos, sem os
        /// caracteres de formatação(ponto, barra, hífen, etc.).</para>
        /// </summary>
        public string IE { get; set; }

        /// <summary>
        /// Inscrição na SUFRAMA
        /// <para>Obrigatório, nas operações que se beneficiam de
        /// incentivos fiscais existentes nas áreas sob controle da
        /// SUFRAMA.</para>
        /// <para>A omissão desta informação impede o
        /// processamento da operação pelo Sistema de Mercadoria
        /// Nacional da SUFRAMA e a liberação da Declaração de
        /// Ingresso, prejudicando a comprovação do ingresso /
        /// internamento da mercadoria nestas áreas. (v2.0)</para>
        /// </summary>
        /// 
        public string IM { get; set; }
        /// <summary>
        /// Inscrição Municipal do Prestador de Serviço
        /// </summary>
        public string ISUF { get; set; }

        /// <summary>
        /// Email
        /// <para>Campo pode ser utilizado para informar o e-mail de
        /// recepção da NF-e indicada pelo destinatário(v2.0)</para>
        /// </summary>
        public string Email { get; set; }

        /// <summary>
        /// Logradouro
        /// </summary>
        public string xLgr { get; set; }

        /// <summary>
        /// Número
        /// </summary>
        public string nro { get; set; }

        /// <summary>
        /// Complemento
        /// </summary>
        public string xCpl { get; set; }

        /// <summary>
        /// Bairro
        /// </summary>
        public string xBairro { get; set; }

        /// <summary>
        /// Código do município
        /// <para>Utilizar a Tabela do IBGE</para>
        /// </summary>
        public int? cMun { get; set; }

        /// <summary>
        /// Nome do município
        /// </summary>
        public string xMun { get; set; }

        /// <summary>
        /// Sigla da UF
        /// </summary>
        public string UF { get; set; }

        /// <summary>
        /// Código do CEP
        /// <para>Informar os zeros não significativos. (NT 2011/004)</para>
        /// </summary>
        public string CEP { get; set; }

        /// <summary>
        /// Código do País
        /// <para>1058=Brasil</para>
        /// </summary>
        public int? cPais { get; set; }

        /// <summary>
        /// Nome do País
        /// <para>Brasil ou BRASIL</para>
        /// </summary>
        public string xPais { get; set; }

        /// <summary>
        /// Telefone
        /// <para>Preencher com o Código DDD + número do telefone. Nas
        /// operações com exterior é permitido informar o código do
        /// país + código da localidade + número do telefone(v2.0)</para>
        /// </summary>
        public string Fone { get; set; }

        #endregion Properties
    }
}