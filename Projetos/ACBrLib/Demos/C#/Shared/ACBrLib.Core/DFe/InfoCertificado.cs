using System;
using System.Reflection;

namespace ACBrLib.Core.DFe
{
    public class InfoCertificado
    {
        public InfoCertificado()
        {
            NumeroSerie = string.Empty;
            RazaoSocial = string.Empty;
            CNPJ = string.Empty;
            Vencimento = DateTime.MinValue;
            Certificadora = string.Empty;
        }

        public InfoCertificado(string dadosCertificado)
        {
            var dados = dadosCertificado.Split('|');

            NumeroSerie = dados[0];
            RazaoSocial = dados[1];
            CNPJ = dados[2];
            Vencimento = DateTime.Parse(dados[3]);
            Certificadora = dados[4];
        }

        public string NumeroSerie { get; set; }

        public string RazaoSocial { get; set; }

        public string CNPJ { get; set; }

        public DateTime Vencimento { get; set; }

        public string Certificadora { get; set; }

        public override string ToString()
        {
            var ret = "";

            var t = typeof(InfoCertificado);
            var properties = t.GetProperties(BindingFlags.Public | BindingFlags.Instance);
            foreach (var info in properties)
            {
                ret += $"{info.Name} = {info.GetValue(this, null)}{Environment.NewLine}";
            }

            return ret;
        }
    }
}