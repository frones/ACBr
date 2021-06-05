namespace ACBrLib.Core.DFe
{
    public enum OrigemMercadoria
    {
        oeNacional = 0,
        oeEstrangeiraImportacaoDireta = 1,
        oeEstrangeiraAdquiridaBrasil = 2,
        oeNacionalConteudoImportacaoSuperior40 = 3,
        oeNacionalProcessosBasicos = 4,
        oeNacionalConteudoImportacaoInferiorIgual40 = 5,
        oeEstrangeiraImportacaoDiretaSemSimilar = 6,
        oeEstrangeiraAdquiridaBrasilSemSimilar = 7,
        oeNacionalConteudoImportacaoSuperior70 = 8
    }
}