namespace ACBrLib.Boleto
{
    public enum TipoDesconto
    {
        tdNaoConcederDesconto,
        tdValorFixoAteDataInformada,
        tdPercentualAteDataInformada,
        tdValorAntecipacaoDiaCorrido,
        tdValorAntecipacaoDiaUtil,
        tdPercentualSobreValorNominalDiaCorrido,
        tdPercentualSobreValorNominalDiaUtil,
        tdCancelamentoDesconto
    }
}