using System;
using System.Collections.Generic;
using ACBrLib.Core.DFe;
using ACBrLib.Core.CTe;

namespace ACBrLib.CTe
{
    public class ComplementoCTe
    {
        private TipoPeriodoCTe _TipoData;
        private TipoHorarioCTe _TipoHorario;

        public string xCaracAd { get; set; }

        public string xCaracSer { get; set; }

        public string xEmi { get; set; }

        [Obsolete("Descontinuado: Para o correto preenchimento, use TipoData ")]
        public TipoPeriodoCTe semData { get; set; }

        [Obsolete("Descontinuado: Para o correto preenchimento, use TipoHora")]
        public TipoHorarioCTe noPeriodo { get; set; }

        [Obsolete("Descontinuado: Para o correto preenchimento, use TipoHora")]
        public TipoHorarioCTe semHora { get; set; }

        public TipoPeriodoCTe TipoData { get => _TipoData; set => SetTipoData(value); }

        public TipoPeriodoCTe tpPer { get => _TipoData; set => _TipoData = value;}

        public TipoHorarioCTe TipoHora { get => _TipoHorario; set => SetTipoHorario(value); } 

        public TipoHorarioCTe tpHor { get => _TipoHorario; set => _TipoHorario = value; }

        public string origCalc { get; set; }

        public string destCalc { get; set; }

        public string xObs { get; set; }

        public DateTime dProg { get; set; }

        public DateTime dIni { get; set; }

        public DateTime dFim { get; set; }

        public String hProg { get; set; }

        public String hIni { get; set; }

        public String hFim { get; set; }

        private void SetTipoData(TipoPeriodoCTe value) => _TipoData = value;
        private void SetTipoHorario(TipoHorarioCTe value) => _TipoHorario = value;
    }
}
