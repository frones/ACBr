using System.Collections.Generic;
using ACBrLib.Core;
using ACBrLib.Core.DFe;

namespace ACBrLib.NFe
{
    public sealed class ConsultaNFeResposta : DFeRespostaBase
    {
        #region Properties

        public string ChNFe { get; set; }

        public string NProt { get; set; }

        public string DigVal { get; set; }

        public int cMsg { get; set; }

        public string xMsg { get; set; }

        public ConsultaNFeInfCanResposta InfCan { get; set; }

        public List<ConsultaNFeProcEventoResposta> Eventos { get; } = new List<ConsultaNFeProcEventoResposta>();

        #endregion Properties

        #region Methods

        public static ConsultaNFeResposta LerResposta(string resposta)
        {
            var iniresposta = ACBrIniFile.Parse(resposta);
            var ret = iniresposta.ReadFromIni<ConsultaNFeResposta>("Consulta");
            ret.Resposta = resposta;
            ret.InfCan = iniresposta.ReadFromIni<ConsultaNFeInfCanResposta>("InfCan");

            var i = 0;
            ConsultaNFeProcEventoResposta item;
            do
            {
                i++;
                item = iniresposta.ReadFromIni<ConsultaNFeProcEventoResposta>($"ProcEventoNFe{i:000}");
                if (item == null) continue;

                item.DetEvento = ConsultaNFeDetEventoResposta.LerRetorno(iniresposta, i);

                var j = 0;
                ConsultaNFeRetEventoResposta retEvento;
                do
                {
                    j++;
                    retEvento = iniresposta.ReadFromIni<ConsultaNFeRetEventoResposta>($"RetEvento{i:000}{j:000}");
                    if (retEvento == null) continue;

                    item.RetEvento.Add(retEvento);
                        
                } while (retEvento != null);

                ret.Eventos.Add(item);
            } while (item != null);

            return ret;
        }

        #endregion Methods
    }
}