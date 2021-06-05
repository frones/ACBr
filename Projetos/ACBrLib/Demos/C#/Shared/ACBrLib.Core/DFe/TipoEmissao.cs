using System;
using System.ComponentModel;

namespace ACBrLib.Core.DFe
{
    public enum TipoEmissao
    {
        [EnumValue("1")]
        [Description("Emissão normal (não em contingência)")]
        teNormal = 0,

        [EnumValue("2")]
        [Description("Contingência FS-IA, com impressão do DANFE em formulário de segurança")]
        teContingencia = 1,

        [EnumValue("3")]
        [Description("Contingência SCAN(Sistema de Contingência do Ambiente Nacional)")]
        teSCAN = 2,

        [EnumValue("4")]
        [Description("Contingência DPEC(Declaração Prévia da Emissão em Contingência)")]
        teDPEC = 3,

        [EnumValue("5")]
        [Description("Contingência FS-DA, com impressão do DANFE em formulário de segurança")]
        teFSDA = 4,

        [EnumValue("6")]
        [Description("Contingência SVC-AN(SEFAZ Virtual de Contingência do AN)")]
        teSVCAN = 5,

        [EnumValue("7")]
        [Description("Contingência SVC-RS(SEFAZ Virtual de Contingência do RS)")]
        teSVCRS = 6,

        [EnumValue("8")]
        [Description("Contingência SVC-SP(SEFAZ Virtual de Contingência de SP)")]
        teSVCSP = 7,

        [EnumValue("9")]
        [Description("Contingência off-line da NFC-e(as demais opções de contingência são válidas também para a NFC-e)")]
        teOffLine = 8
    }
}