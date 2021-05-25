using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace ACBrLib.Core.Config
{
    public abstract class ACBrLibConfigBase<TLib> where TLib : ACBrLibHandle
    {
        #region Fields

        protected readonly TLib Parent;
        protected readonly ACBrSessao SessaoConfig;
        protected string SubName;

        #endregion Fields

        #region Constructors

        protected ACBrLibConfigBase(TLib acbrlib, ACBrSessao sessao)
        {
            Parent = acbrlib;
            SessaoConfig = sessao;
        }

        #endregion Constructors

        #region Methods

        protected TProp GetProperty<TProp>([CallerMemberName] string propertyName = null)
        {
            return Parent.ConfigLerValor<TProp>(SessaoConfig, string.IsNullOrEmpty(SubName) ? propertyName : $"{SubName}.{propertyName}");
        }

        protected void SetProperty<TProp>(TProp newValue, [CallerMemberName] string propertyName = null)
        {
            Parent.ConfigGravarValor(SessaoConfig, string.IsNullOrEmpty(SubName) ? propertyName : $"{SubName}.{propertyName}", newValue);
        }

        #endregion Methods
    }
}