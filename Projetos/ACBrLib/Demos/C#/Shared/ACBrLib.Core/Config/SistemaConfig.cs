using System;

namespace ACBrLib.Core.Config
{
    public sealed class SistemaConfig<TLib> : ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Constructors

        public SistemaConfig(TLib acbrlib) : base(acbrlib, ACBrSessao.Sistema)
        {
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Versao
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public DateTime Data
        {
            get => GetProperty<DateTime>();
            set => SetProperty(value);
        }

        public string Descricao
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}