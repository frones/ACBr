using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using ACBrLib.Core;

namespace ACBrLib.Boleto
{
    public sealed class RetornoBoleto
    {
        #region Properties

        public RetornoCedente Cedente { get; } = new RetornoCedente();

        public RetornoBanco Banco { get; } = new RetornoBanco();

        public RetornoConta Conta { get; } = new RetornoConta();

        public List<RetornoTitulo> Titulos { get; } = new List<RetornoTitulo>();

        public string Retorno { get; set; }

        #endregion Properties

        #region Methods

        public static RetornoBoleto LerRetorno(string retorno)
        {
            var iniFile = ACBrIniFile.Parse(retorno);

            var ret = new RetornoBoleto();
            iniFile.ReadFromIni(ret.Cedente, "CEDENTE");
            iniFile.ReadFromIni(ret.Banco, "BANCO");
            iniFile.ReadFromIni(ret.Conta, "CONTA");

            foreach (var section in iniFile.Where(x => x.Name.StartsWith("TITULO")))
            {
                var item = new RetornoTitulo();
                section.ReadFromINi(item);

                var id = Regex.Replace(section.Name, "[^0-9]", string.Empty);
                if (string.IsNullOrEmpty(id)) continue;

                foreach (var sec in iniFile.Where(x => x.Name.StartsWith($"MotivoRejeicao{id}-")))
                {
                    var rejeicao = new RetornoRejeicao();
                    sec.ReadFromINi(rejeicao);
                    item.Rejeicoes.Add(rejeicao);
                }

                ret.Titulos.Add(item);
            }

            return ret;
        }

        #endregion Methods
    }
}