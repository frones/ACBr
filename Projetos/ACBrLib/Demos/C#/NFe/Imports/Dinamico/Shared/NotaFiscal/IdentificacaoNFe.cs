using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.NFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// Identificação da Nota Fiscal eletrônica
    /// </summary>
    public class IdentificacaoNFe
    {
        #region Properties

        /// <sumary> 
        /// Código da UF do emitente do Documento Fiscal 
        /// </sumary>

        public int cUF { get; set; }

        /// <summary>
        /// Código Numérico que compõe a Chave de Acesso
        /// </summary>
        public int cNF { get; set; }

        /// <summary>
        /// Descrição da Natureza da Operação
        /// </summary>
        public string natOp { get; set; }

        /// <summary>
        /// Indicador da forma de pagamento
        /// <para>0=Pagamento à vista;</para>
        /// <para>1=Pagamento a prazo;</para>
        /// <para>2=Outros.</para>
        /// </summary>
        public IndicadorPagamento indPag { get; set; }

        /// <summary>
        /// Código do Modelo do Documento Fiscal
        /// </summary>
        public ModeloNFe modelo { get; set; }

        /// <summary>
        /// Série do Documento Fiscal
        /// </summary>
        public string Serie { get; set; }

        /// <summary>
        /// Número do Documento Fiscal
        /// </summary>
        public int nNF { get; set; }

        /// <summary>
        /// Data e hora de emissão do Documento Fiscal
        /// </summary>
        public DateTime dhEmi { get; set; }

        /// <summary>
        /// Data e hora de Saída ou da Entrada da Mercadoria/Produto
        /// </summary>
        public DateTime? dhSaiEnt { get; set; }

        /// <summary>
        /// Tipo de Operação
        /// <para>0=Entrada;</para>
        /// <para>1=Saída</para>
        /// </summary>
        public TipoNFe tpNF { get; set; }

        /// <summary>
        /// Identificador de local de destino da operação
        /// <para>1=Operação interna;</para>
        /// <para>2=Operação interestadual;</para>
        /// <para>3=Operação com exterior.</para>
        /// </summary>
        public DestinoOperacao idDest { get; set; }

        /// <summary>
        /// Formato de Impressão do DANFE
        /// <para>0=Sem geração de DANFE;</para>
        /// <para>1=DANFE normal, Retrato;</para>
        /// <para>2=DANFE normal, Paisagem;</para>
        /// <para>3=DANFE Simplificado;</para>
        /// <para>4=DANFE NFC-e;</para>
        /// <para>5=DANFE NFC-e em mensagem eletrônica(o envio de
        /// mensagem eletrônica pode ser feita de forma
        /// simultânea com a impressão do DANFE; usar o tpImp=5
        /// quando esta for a única forma de disponibilização do
        /// DANFE).</para>
        /// </summary>
        public TipoDANFE tpImp { get; set; }

        /// <summary>
        /// Tipo de Emissão da NF-e
        /// <para>1=Emissão normal (não em contingência);</para>
        /// <para>2=Contingência FS-IA, com impressão do DANFE em
        /// formulário de segurança;</para>
        /// <para>3=Contingência SCAN(Sistema de Contingência do
        /// Ambiente Nacional);</para>
        /// <para>4=Contingência DPEC(Declaração Prévia da Emissão
        /// em Contingência);</para>
        /// <para>5=Contingência FS-DA, com impressão do DANFE em
        /// formulário de segurança;</para>
        /// <para>6=Contingência SVC-AN(SEFAZ Virtual de Contingência
        /// do AN);</para>
        /// <para>7=Contingência SVC-RS(SEFAZ Virtual de Contingência
        /// do RS);</para>
        /// <para>9=Contingência off-line da NFC-e(as demais opções de
        /// contingência são válidas também para a NFC-e);</para>
        /// <para>Observação: Para a NFC-e somente estão disponíveis e
        /// são válidas as opções de contingência 5 e 9.</para>
        /// </summary>
        public TipoEmissao tpEmis { get; set; }

        public TipoAmbiente tpAmb { get; set; }

        /// <summary>
        /// Finalidade de emissão da NF-e
        /// <para>1=NF-e normal;</para>
        /// <para>2=NF-e complementar;</para>
        /// <para>3=NF-e de ajuste;</para>
        /// <para>4=Devolução de mercadoria.</para>
        /// </summary>
        public FinalidadeNFe finNFe { get; set; }

        /// <summary>
        /// Indica operação com Consumidor final
        /// <para>0=Normal;</para>
        /// <para>1=Consumidor final;</para>
        /// </summary>
        public ConsumidorFinal indFinal { get; set; }

        /// <summary>
        /// Indicador de presença do comprador no estabelecimento comercial no momento da operação
        /// <para>0=Não se aplica (por exemplo, Nota Fiscal complementar
        /// ou de ajuste);</para>
        /// <para>1=Operação presencial;</para>
        /// <para>2=Operação não presencial, pela Internet;</para>
        /// <para>3=Operação não presencial, Teleatendimento;</para>
        /// <para>4=NFC-e em operação com entrega a domicílio;</para>
        /// <para>9=Operação não presencial, outros.</para>
        /// </summary>
        public PresencaComprador indPres { get; set; }

        /// <summary>
        /// Processo de emissão da NF-e E B01 N 1-1 1 0=Emissão de NF-e com aplicativo do contribuinte
        /// <para>0=Emissão de NF-e com aplicativo do contribuinte;</para>
        /// <para>1=Emissão de NF-e avulsa pelo Fisco;</para>
        /// <para>2=Emissão de NF-e avulsa, pelo contribuinte com seu
        /// certificado digital, através do site do Fisco;</para>
        /// <para>3=Emissão NF-e pelo contribuinte com aplicativo
        /// fornecido pelo Fisco.</para>
        /// </summary>
        public ProcessoEmissao procEmi { get; set; }

        public IndIntermed indIntermed { get; set; }

        /// <summary>
        /// Versão do Processo de emissão da NF-e
        /// </summary>
        public string verProc { get; set; }

        /// <summary>
        /// Data e Hora da entrada em contingência
        /// </summary>
        public DateTime? dhCont { get; set; }

        /// <summary>
        /// Justificativa da entrada em contingência
        /// </summary>
        public string xJust { get; set; }

        /// <summary>
        /// O Grupo "NFref" deve ser preenchido apenas se existir documentos Referenciados na NFe (Tipos aceitos: NFE, CTE, ECF, NFP, NF)
        /// </summary>
        public List<NFRef> NFref { get; } = new List<NFRef>();

        /// <summary>
        /// Código do Município de Ocorrência do Fato Gerador
        /// </summary>
        public int cMunFG { get; set; }

        #endregion Properties
    }
}