using ACBrLib.Core;
using ACBrLib.Core.Config;
using ACBrLib.Core.Mail;

namespace ACBrLib.Mail
{
    public sealed class MailConfig : ACBrLibConfig<ACBrMail>
    {
        #region Constructors

        public MailConfig(ACBrMail acbrlib) : base(acbrlib, ACBrSessao.Email)
        {
        }

        #endregion Constructors

        #region Properties

        public string Nome
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Servidor
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Conta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Usuario
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public string Senha
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public MimeChar Codificacao
        {
            get => GetProperty<MimeChar>();
            set => SetProperty(value);
        }

        public string Porta
        {
            get => GetProperty<string>();
            set => SetProperty(value);
        }

        public bool SSL
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool TLS
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int TimeOut
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool Confirmacao
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public bool SegundoPlano
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public int Tentativas
        {
            get => GetProperty<int>();
            set => SetProperty(value);
        }

        public bool IsHTML
        {
            get => GetProperty<bool>();
            set => SetProperty(value);
        }

        public MessPriority Priority
        {
            get => GetProperty<MessPriority>();
            set => SetProperty(value);
        }

        #endregion Properties
    }
}