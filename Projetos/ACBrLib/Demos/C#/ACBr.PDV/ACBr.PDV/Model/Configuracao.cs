using System;
using System.Configuration;
using System.Globalization;
using System.IO;

namespace ACBr.PDV.Model
{
    public class Configuracao
    {
        #region Fields

        public static Configuracao Instance;

        private const string ConfigFile = "acbr.config";
        private Configuration config;

        #endregion Fields

        #region Constructors

        static Configuracao()
        {
            Instance = CreateOrLoad();
        }

        private Configuracao(Configuration config)
        {
            this.config = config;
            Emitente = new Emitente();
            DFe = new DocumentoFiscal();
            LoadInfo();
        }

        #endregion Constructors

        #region Properties

        public Emitente Emitente { get; }

        public DocumentoFiscal DFe { get; }

        #endregion Properties

        #region Methods

        public void Save()
        {
            Set("Emitente.CNPJ", Emitente.CNPJ);
            Set("Emitente.IE", Emitente.IE);
            Set("Emitente.IM", Emitente.IM);
            Set("Emitente.Razao", Emitente.Razao);
            Set("Emitente.Fantasia", Emitente.Fantasia);
            Set("Emitente.Fone", Emitente.Fone);
            Set("Emitente.CEP", Emitente.CEP);
            Set("Emitente.Logradouro", Emitente.Logradouro);
            Set("Emitente.Numero", Emitente.Numero);
            Set("Emitente.Complemento", Emitente.Complemento);
            Set("Emitente.Bairro", Emitente.Bairro);
            Set("Emitente.CidadeCod", Emitente.CidadeCod);
            Set("Emitente.Cidade", Emitente.Cidade);
            Set("Emitente.UF", Emitente.UF);
            Set("Emitente.CRT", Emitente.CRT);

            Set("DFe.TipoDocumento", DFe.TipoDocumento);
            Set("DFe.Serie", DFe.Serie);
            Set("DFe.NumeroAtual", DFe.NumeroAtual);

            config.Save(ConfigurationSaveMode.Minimal, true);
        }

        public void Load()
        {
            var configFileMap = new ExeConfigurationFileMap
            {
                ExeConfigFilename = ConfigFile
            };

            config = ConfigurationManager.OpenMappedExeConfiguration(configFileMap, ConfigurationUserLevel.None);
            LoadInfo();
        }

        private void LoadInfo()
        {
            Emitente.Clear();
            Emitente.CNPJ = Get("Emitente.CNPJ", Emitente.CNPJ);
            Emitente.IE = Get("Emitente.IE", Emitente.IE);
            Emitente.IM = Get("Emitente.IM", Emitente.IM);
            Emitente.Razao = Get("Emitente.Razao", Emitente.Razao);
            Emitente.Fantasia = Get("Emitente.Fantasia", Emitente.Fantasia);
            Emitente.Fone = Get("Emitente.Fone", Emitente.Fone);
            Emitente.CEP = Get("Emitente.CEP", Emitente.CEP);
            Emitente.Logradouro = Get("Emitente.Logradouro", Emitente.Logradouro);
            Emitente.Numero = Get("Emitente.Numero", Emitente.Numero);
            Emitente.Complemento = Get("Emitente.Complemento", Emitente.Complemento);
            Emitente.Bairro = Get("Emitente.Bairro", Emitente.Bairro);
            Emitente.CidadeCod = Get("Emitente.CidadeCod", Emitente.CidadeCod);
            Emitente.Cidade = Get("Emitente.Cidade", Emitente.Cidade);
            Emitente.UF = Get("Emitente.UF", Emitente.UF);
            Emitente.CRT = Get("Emitente.CRT", Emitente.CRT);

            DFe.Clear();
            DFe.TipoDocumento = Get("DFe.TipoDocumento", DFe.TipoDocumento);
            DFe.Serie = Get("DFe.Serie", DFe.Serie);
            DFe.NumeroAtual = Get("DFe.NumeroAtual", DFe.NumeroAtual);
        }

        private static Configuracao CreateOrLoad()
        {
            if (!File.Exists(ConfigFile))
            {
                var config = "<?xml version='1.0' encoding='utf-8' ?>" + Environment.NewLine +
                             "<configuration>" + Environment.NewLine +
                             "    <appSettings>" + Environment.NewLine +
                             "    </appSettings>" + Environment.NewLine +
                             "</configuration>";
                File.WriteAllText(ConfigFile, config);
            }

            var configFileMap = new ExeConfigurationFileMap
            {
                ExeConfigFilename = ConfigFile
            };

            var configuration = ConfigurationManager.OpenMappedExeConfiguration(configFileMap, ConfigurationUserLevel.None);
            return new Configuracao(configuration);
        }

        private void Set(string setting, object value)
        {
            var valor = string.Format(CultureInfo.InvariantCulture, "{0}", value);

            if (config.AppSettings.Settings[setting]?.Value != null)
                config.AppSettings.Settings[setting].Value = valor;
            else
                config.AppSettings.Settings.Add(setting, valor);
        }

        private T Get<T>(string setting, T defaultValue)
        {
            var type = typeof(T);
            var value = config.AppSettings.Settings[setting]?.Value;
            if (string.IsNullOrEmpty(value)) return defaultValue;

            try
            {
                if (type.IsEnum || type.IsGenericType && type.GetGenericArguments()[0].IsEnum)
                {
                    return (T)Enum.Parse(type, value);
                }

                return (T)Convert.ChangeType(value, type, CultureInfo.InvariantCulture);
            }
            catch (Exception)
            {
                return defaultValue;
            }
        }

        #endregion Methods
    }
}