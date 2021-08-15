using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.MDFe
{
    public sealed class NaoEncerradosResposta : DFeRespostaBase
    {
        #region Properties

        public List<NaoEncerradosRespostaItem> Items { get; } = new List<NaoEncerradosRespostaItem>();

        #endregion Properties

        #region Methods

        public static NaoEncerradosResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<NaoEncerradosResposta>("NAOENCERRADOS");
            ret.Resposta = resposta;

            var i = 0;
            NaoEncerradosRespostaItem item;
            do
            {
                i++;
                item = iniresposta.ReadFromIni<NaoEncerradosRespostaItem>($"NAOENCERRADOS{i:000}");
                if (item == null) continue;

                ret.Items.Add(item);
            } while (item != null);

            return ret;
        }

        #endregion Methods
    }
}