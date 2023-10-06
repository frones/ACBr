using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    /// <summary>
    /// ICMS Normal e ST
    /// </summary>
    public class ICMSProdutoNFe
    {
        #region Properties

        /// <summary>
        /// Origem da mercadoria
        /// <para>0 - Nacional, exceto as indicadas nos códigos 3, 4, 5 e 8;</para>
        /// <para>1 - Estrangeira - Importação direta, exceto a indicada no código 6;</para>
        /// <para>2 - Estrangeira - Adquirida no mercado interno, exceto a indicada no código 7;</para>
        /// <para>3 - Nacional, mercadoria ou bem com Conteúdo de Importação superior a 40% e inferior ou igual a 70%;</para>
        /// <para>4 - Nacional, cuja produção tenha sido feita em conformidade com os processos produtivos básicos de que tratam as legislações citadas nos Ajustes;</para>
        /// <para>5 - Nacional, mercadoria ou bem com Conteúdo de Importação inferior ou igual a 40%;</para>
        /// <para>6 - Estrangeira - Importação direta, sem similar nacional, constante em lista da CAMEX e gás natural;</para>
        /// <para>7 - Estrangeira - Adquirida no mercado interno, sem similar nacional, constante lista CAMEX e gás natural.</para>
        /// <para>8 - Nacional, mercadoria ou bem com Conteúdo de Importação superior a 70%;</para>
        /// </summary>
        public OrigemMercadoria orig { get; set; }

        /// <summary>
        /// Tributação do ICMS = X
        /// <para>X = Com redução de base de cálculo</para>
        /// <para>00=Tributada integralmente (requerido modBC, vBC, pICMS, vICMS)</para>
        /// <para>10=Tributada e com cobrança do ICMS por substituição tributária (requerido modBC, vBC, pICMS, vICMS, modBCST, vBCST, pICMSST, vICMSST)</para>
        /// <para>20=Com redução de base de cálculo (requerido modBC, vBC, pICMS, vICMS, pRedBC)</para>
        /// <para>30=Isenta ou não tributada e com cobrança do ICMS por substituição tributária (requerido modBCST, vBCST, pICMSST, vICMSST)</para>
        /// <para>40=Isenta; 41=Não tributada; 50=Suspensão. (requerido vICMSDeson, motDesICMS)</para>
        /// <para>51=Diferimento</para>
        /// <para>60=ICMS cobrado anteriormente por substituição tributária (requerido vBCSTRet, vICMSSTRet)</para>
        /// <para>70=Com redução de base de cálculo e cobrança do ICMS por substituição tributária (requerido modBC, pRedBC, vBC, pICMS, vICMS, modBCST, vBCST, pICMSST, vICMSST, vICMSDeson, motDesICMS)</para>
        /// <para>90=Outros (requerido modBC, vBC, pICMS, vICMS, modBCST, vBCST, pICMSST, vICMSST)</para>
        /// </summary>
        public CSTIcms CST { get; set; }

        public CSOSNIcms CSOSN { get; set; }

        /// <summary>
        /// Modalidade de determinação da BC do ICMS
        /// <para>0=Margem Valor Agregado (%);</para>
        /// <para>1=Pauta(Valor);</para>
        /// <para>2=Preço Tabelado Máx. (valor);</para>
        /// <para>3=Valor da operação.</para>
        /// </summary>
        public DeterminacaoBaseIcms modBC { get; set; }

        /// <summary>
        /// Percentual da Redução de BC
        /// </summary>
        public decimal? pRedBC { get; set; }

        /// <summary>
        /// Valor da BC do ICMS
        /// </summary>
        public decimal? vBC { get; set; }

        /// <summary>
        /// Alíquota do imposto
        /// </summary>
        public decimal? pICMS { get; set; }

        /// <summary>
        /// Valor do ICMS
        /// </summary>
        public decimal? vICMS { get; set; }

        /// <summary>
        /// Modalidade de determinação da BC do ICMS ST
        /// <para>0=Preço tabelado ou máximo sugerido;</para>
        /// <para>1=Lista Negativa(valor);</para>
        /// <para>2=Lista Positiva(valor);</para>
        /// <para>3=Lista Neutra(valor);</para>
        /// <para>4=Margem Valor Agregado(%);</para>
        /// <para>5=Pauta(valor);</para>
        /// </summary>
        public DeterminacaoBaseIcmsST modBCST { get; set; }

        /// <summary>
        /// Percentual da margem de valor Adicionado do ICMS ST
        /// </summary>
        public decimal? pMVAST { get; set; }

        /// <summary>
        /// Percentual da Redução de BC do ICMS ST
        /// </summary>
        public decimal? pRedBCST { get; set; }

        /// <summary>
        /// Valor da BC do ICMS ST
        /// </summary>
        public decimal? vBCST { get; set; }

        /// <summary>
        /// Alíquota do imposto do ICMS ST
        /// </summary>
        public decimal? pICMSST { get; set; }

        /// <summary>
        /// Valor do ICMS ST retido
        /// </summary>
        public decimal? vICMSST { get; set; }

        /// <summary>
        /// Sigla da UF para qual é devido o ICMS ST da operação. Informar "EX" para Exterior. (v2.0)
        /// </summary>
        public decimal? UFST { get; set; }

        /// <summary>
        /// Percentual para determinação do valor da Base de Cálculo da operação própria.
        /// </summary>
        public decimal? pBCOp { get; set; }

        /// <summary>
        /// Valor da BC do ICMS ST retido
        /// <para>Valor da BC do ICMS ST cobrado anteriormente por ST (v2.0).</para>
        /// <para>O valor pode ser omitido quando a legislação não exigir a sua informação. (NT 2011/004)</para>
        /// </summary>
        public decimal? vBCSTRet { get; set; }

        /// <summary>
        /// Valor do ICMS ST retido
        /// <para>Valor do ICMS ST cobrado anteriormente por ST (v2.0).</para>
        /// <para>O valor pode ser omitido quando a legislação não exigir a sua informação. (NT 2011/004)</para>
        /// </summary>
        public decimal? vICMSSTRet { get; set; }

        /// <summary>
        /// Motivo da desoneração do ICMS
        /// <para>Campo será preenchido quando o campo anterior estiver preenchido.Informar o motivo da desoneração:</para>
        /// <para>1=Táxi;</para>
        /// <para>3=Produtor Agropecuário;</para>
        /// <para>4=Frotista/Locadora;</para>
        /// <para>5=Diplomático/Consular;</para>
        /// <para>6=Utilitários e Motocicletas da Amazônia Ocidental e Áreas de Livre Comércio(Resolução 714/88 e 790/94 – CONTRAN e suas alterações);</para>
        /// <para>7=SUFRAMA;</para>
        /// <para>8=Venda a Órgão Público;</para>
        /// <para>9=Outros. (NT 2011/004);</para>
        /// <para>10=Deficiente Condutor(Convênio ICMS 38/12);</para>
        /// <para>11=Deficiente Não Condutor(Convênio ICMS 38/12).</para>
        /// <para>12=Órgão de fomento e desenvolvimento agropecuário.</para>
        /// <para>Observação: Revogada a partir da versão 3.01 a possibilidade de usar o motivo 2=Deficiente Físico</para>
        /// <para>Observação ICMS=20,70,90: apenas 3, 9 e 12 podem ser usados</para>
        /// <para>Observação ICMS=30: apenas 6, 7 e 9 podem ser usados</para>
        /// <para>Observação ICMS=40,41,50: de 1 a 11 podem ser usados</para>
        /// </summary>
        public MotivoDesoneracaoICMS motDesICMS { get; set; }

        /// <summary>
        /// Valor do ICMS
        /// <para>Informar apenas nas operações:</para>
        /// <para>a) com produtos beneficiados com a desoneração condicional do ICMS.</para>
        /// <para>b) destinadas à SUFRAMA, informando-se o valor que seria devido se não houvesse isenção.</para>
        /// <para>c) de venda a órgão da administração pública direta e suas fundações e autarquias com isenção do ICMS. (NT 2011/004)</para>
        /// </summary>
        public decimal? vICMSDeson { get; set; }

        /// <summary>
        /// Alíquota aplicável de cálculo do crédito (Simples Nacional).
        /// </summary>
        public decimal? pCredSN { get; set; }

        /// <summary>
        /// Valor crédito do ICMS que pode ser aproveitado nos termos do art. 23 da LC 123 (Simples Nacional)
        /// </summary>
        public decimal? vCredICMSSN { get; set; }

        /// <summary>
        /// Valor da BC do ICMS ST da UF destino
        /// </summary>
        public decimal? vBCSTDest { get; set; }

        /// <summary>
        /// Valor do ICMS ST da UF destino
        /// </summary>
        public decimal? vICMSSTDest { get; set; }

        /// <summary>
        /// Valor do ICMS da Operação.
        /// Valor como se não tivesse o diferimento
        /// </summary>
        public decimal? vICMSOp { get; set; }

        /// <summary>
        /// Percentual do diferimento
        /// <para>No caso de diferimento total, informar o percentual de diferimento "100".</para>
        /// </summary>
        public decimal? pDif { get; set; }

        /// <summary>
        /// Valor do ICMS diferido
        /// </summary>
        public decimal? vICMSDif { get; set; }

        public decimal? pST { get; set; }

        public decimal? vBCFCP { get; set; }

        public decimal? pFCP { get; set; }

        public decimal? vFCP { get; set; }

        public decimal? vBCFCPST { get; set; }

        public decimal? pFCPST { get; set; }

        public decimal? vFCPST { get; set; }

        public decimal? vBCFCPSTRet { get; set; }

        public decimal? pFCPSTRet { get; set; }

        public decimal? vFCPSTRet { get; set; }

        public decimal? pRedBCEfet { get; set; }

        public decimal? vBCEfet { get; set; }

        public decimal? pICMSEfet { get; set; }

        public decimal? vICMSEfet { get; set; }

        public decimal? vICMSSubstituto { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto
        /// </summary>
        public decimal? adRemICMS { get; set; }

        /// <summary>
        /// Valor do ICMS próprio
        /// </summary>
        public decimal? vICMSMono { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto com retenção
        /// </summary>
        public decimal? adRemICMSReten { get; set; }

        /// <summary>
        /// Valor do ICMS com retenção
        /// </summary>
        public decimal? vICMSMonoReten { get; set; }

        /// <summary>
        /// Valor do ICMS diferido
        /// </summary>
        public decimal? vICMSMonoDif { get; set; }

        /// <summary>
        /// Alíquota ad rem do imposto retido anteriormente
        /// </summary>
        public decimal? adRemICMSRet { get; set; }

        public decimal? vICMSMonoRet { get; set; }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico próprio
        /// </summary>
        public decimal? qBCMono { get; set; }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico sujeito a retenção
        /// </summary>
        public decimal? qBCMonoReten { get; set; }

        /// <summary>
        /// Percentual de redução do valor da alíquota adRem
        /// </summary>
        public decimal? pRedAdRem { get; set; }

        /// <summary>
        /// Motivo da redução do adRem
        /// </summary>
        public MotRedAdRem motRedAdRem { get; set; }

        /// <summary>
        /// Valor total da quantidade tributada do ICMS monofásico retido anteriormente
        /// </summary>
        public decimal? qBCMonoRet { get; set; }

        /// <summary>
        /// Valor do ICMS da operação 
        /// </summary>
        public decimal? vICMSMonoOp { get; set; }
        
        /// <summary>
        /// Percentual do diferimento do ICMS relativo ao Fundo de Combate à Pobreza(FCP)
        /// </summary>
        public decimal? pFCPDif { get; set; }

        /// <summary>
        /// Valor do ICMS relativo ao Fundo de Combate à Pobreza (FCP) diferido
        /// </summary>
        public decimal? vFCPDif { get; set; }

        /// <summary>
        /// Valor efetivo do ICMS relativo ao Fundo de Combate à Pobreza(FCP)
        /// </summary>
        ///
        public decimal? vFCPEfet { get; set; }
        #endregion Properties
    }
}