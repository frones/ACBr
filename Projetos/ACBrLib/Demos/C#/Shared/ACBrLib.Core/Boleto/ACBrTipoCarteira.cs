namespace ACBrLib.Core.Boleto
{
    public enum ACBrTipoCarteira
    {
        tctSimples = 0,
        tctRegistrada = 1,
        tctEletronica = 2
    }

    public enum CaracTitulo
    {
        tcSimples,
        tcVinculada,
        tcCaucionada,
        tcDescontada,
        tcVendor,
        tcDireta,
        tcSimplesRapComReg,
        tcCaucionadaRapComReg,
        tcDiretaEspecial
    }

    public enum ACBrBoletoFiltro
    {
        fiNenhum,
        fiPDF,
        fiHTML,
        fiJPG
    }
}