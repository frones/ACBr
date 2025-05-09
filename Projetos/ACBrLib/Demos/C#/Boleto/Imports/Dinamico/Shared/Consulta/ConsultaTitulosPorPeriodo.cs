using System;
using System.Globalization;
using ACBrLib.Core;
using ACBrLib.Core.Boleto;
namespace ACBrLib.Boleto
{
    public class ConsultaTitulosPorPeriodo
    {
        public ACBrIndicadorSituacaoBoleto IndicadorSituacaoBoleto { get; set; }
        public ACBrIndicadorBoletoVencido? BoletoVencido { get; set; }
        public DateTime? DataInicioRegistro { get; set; }
        public DateTime? DataFinalRegistro { get; set; }
        public DateTime? DataInicioMovimento { get; set; }
        public DateTime? DataFinalMovimento { get; set; }
        public DateTime? DataFinalVencimento { get; set; }
        public DateTime? DataInicioVencimento { get; set; }
        public string CnpjCpfPagador { get; set; }
        public int? ContaCaucao { get; set; }
        public int? CodigoEstadoTituloCobranca { get; set; }
        public int? ModalidadeCobranca { get; set; }
        public int? Carteira { get; set; }
        public int? CarteiraVariacao { get; set; }
        public int? IndiceContinuidade { get; set; }
        public int? NumeroProtocolo { get; set; }
        public int? Identificador { get; set; }

        public string ToIniString()
        {
            var iniFile = new ACBrIniFile();
            string section = "ConsultaAPI";

            if (this.IndicadorSituacaoBoleto == ACBrIndicadorSituacaoBoleto.isbNenhum)
            {
                throw new InvalidOperationException("A propriedade 'IndicadorSituacaoBoleto' deve ser definida com um valor válido (isbAberto, isbBaixado ou isbCancelado)");
            }

            iniFile[section]["IndicadorSituacaoBoleto"] = this.IndicadorSituacaoBoleto.ToString("D");

            WriteOptionalEnum(iniFile, section, "BoletoVencido", this.BoletoVencido);

            WriteOptionalValue(iniFile, section, "ContaCaucao", this.ContaCaucao);
            WriteOptionalValue(iniFile, section, "CodigoEstadoTituloCobranca", this.CodigoEstadoTituloCobranca);
            WriteOptionalValue(iniFile, section, "Carteira", this.Carteira);
            WriteOptionalValue(iniFile, section, "CarteiraVariacao", this.CarteiraVariacao);
            WriteOptionalValue(iniFile, section, "ModalidadeCobranca", this.ModalidadeCobranca);

            WriteOptionalDateRange(iniFile, section, "Registro", this.DataInicioRegistro, this.DataFinalRegistro, "dd/MM/yyyy");
            WriteOptionalDateRange(iniFile, section, "Movimento", this.DataInicioMovimento, this.DataFinalMovimento, "dd/MM/yyyy");
            WriteOptionalDateRange(iniFile, section, "Vencimento", this.DataInicioVencimento, this.DataFinalVencimento, "dd/MM/yyyy");

            WriteOptionalString(iniFile, section, "cnpjCpfPagador", this.CnpjCpfPagador);

            WriteOptionalValue(iniFile, section, "IndiceContinuidade", this.IndiceContinuidade);
            WriteOptionalValue(iniFile, section, "NumeroProtocolo", this.NumeroProtocolo);
            WriteOptionalValue(iniFile, section, "Identificador", this.Identificador);

            return iniFile.ToString();
        }

        private static void WriteOptionalValue<T>(ACBrIniFile iniFile, string section, string key, T? value) where T : struct
        {
            if (value.HasValue)
            {
                iniFile[section][key] = value.Value.ToString();
            }
        }

        private static void WriteOptionalEnum<T>(ACBrIniFile iniFile, string section, string key, T? value) where T : struct, Enum
        {
            if (value.HasValue)
            {
                iniFile[section][key] = value.Value.ToString("D");
            }
        }

        private static void WriteOptionalString(ACBrIniFile iniFile, string section, string key, string value)
        {
            if (!string.IsNullOrWhiteSpace(value))
            {
                iniFile[section][key] = value;
            }
        }

        private static void WriteOptionalDateRange(ACBrIniFile iniFile, string section, string periodType, DateTime? startDate, DateTime? endDate, string dateFormat)
        {
            if (startDate.HasValue && endDate.HasValue)
            {
                iniFile[section]["DataInicio" + periodType] = startDate.Value.ToString(dateFormat, CultureInfo.InvariantCulture);
                iniFile[section]["DataFinal" + periodType] = endDate.Value.ToString(dateFormat, CultureInfo.InvariantCulture);
            }
        }

        public override string ToString()
        {
            return ToIniString();
        }
    }
}
