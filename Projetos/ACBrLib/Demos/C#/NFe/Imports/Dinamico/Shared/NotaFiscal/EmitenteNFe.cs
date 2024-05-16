using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Identificação do Emitente da Nota Fiscal eletrônica
    /// </summary>
    public class EmitenteNFe
    {
        #region Properties

        /// <summary>
        /// Código de Regime Tributário
        /// <para>1=Simples Nacional;</para>
        /// <para>2=Simples Nacional, excesso sublimite de receita bruta;</para>
        /// <para>3=Regime Normal. (v2.0).</para>
        /// </summary>
        public CRT CRT { get; set; }

        /// <summary>
        /// CNPJ ou CPF do destinatário
        /// </summary>
        public string CNPJCPF { get; set; }

        /// <summary>
        /// Razão Social ou Nome do emitente
        /// </summary>
        public string xNome { get; set; }

        /// <summary>
        /// Nome fantasia
        /// </summary>
        public string xFant { get; set; }

        /// <summary>
        /// Inscrição Estadual do Emitente
        /// <para>Informar somente os algarismos, sem os caracteres de
        /// formatação(ponto, barra, hífen, etc.).</para>
        /// <para>Observação: Na emissão de NF-e Avulsa pode ser
        /// o literal "ISENTO" para os contribuintes do
        /// ICMS isentos de inscrição no Cadastro de
        /// Contribuintes de ICMS.</para>
        /// </summary>
        public string IE { get; set; }

        /// <summary>
        /// IE do Substituto Tributário
        /// <para>IE do Substituto Tributário da UF de destino da
        /// mercadoria, quando houver a retenção do ICMS ST para
        /// a UF de destino.</para>
        /// </summary>
        public string IEST { get; set; }

        /// <summary>
        /// Inscrição Municipal do Prestador de	Serviço
        /// <para>Informado na emissão de NF-e conjugada, com itens de
        /// produtos sujeitos ao ICMS e itens de serviços sujeitos ao ISSQN.</para>
        /// </summary>
        public string IM { get; set; }

        /// <summary>
        /// CNAE fiscal
        /// <para>Campo Opcional. Pode ser informado quando a Inscrição
        /// Municipal(id:C19) for informada.</para>
        /// </summary>
        public string CNAE { get; set; }

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
        public int cMun { get; set; }

        /// <summary>
        /// Nome do município
        /// </summary>
        public string xMun { get; set; }

        /// <summary>
        /// Código da UF do emitente do Documento Fiscal
        /// </summary>
        public string cUF { get; set; }

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
        public int cPais { get; set; }

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