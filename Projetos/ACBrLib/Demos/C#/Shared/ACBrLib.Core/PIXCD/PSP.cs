using System.ComponentModel;

namespace ACBrLib.Core.PIXCD
{
    public enum PSP
    {
        [Description("Bradesco")]
        pspBradesco = 0,

        [Description("Itaú")]
        pspItau = 1,

        [Description("Banco do Brasil")]
        pspBancoBrasil = 2,

        [Description("Santander")]
        pspSantander = 3,

        [Description("Shipay")]
        pspShipay = 4,

        [Description("Sicredi")]
        pspSicredi = 5,

        [Description("Sicoob")]
        pspSicoob = 6,

        [Description("PagSeguro")]
        pspPagSeguro = 7,

        [Description("GerenciaNet")]
        pspGerenciaNet = 8,

        [Description("PixPDV")]
        pspPixPDV = 9,

        [Description("Inter")]
        pspInter = 10,

        [Description("Ailos")]
        pspAilos = 11,

        [Description("Matera")]
        pspMatera = 12,

        [Description("Cielo")]
        pspCielo = 13,

        [Description("Mercado Pago")]
        pspMercadoPago = 14,
    }
}