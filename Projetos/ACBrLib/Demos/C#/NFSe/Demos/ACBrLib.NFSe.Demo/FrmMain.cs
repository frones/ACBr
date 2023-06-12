using System;
using System.IO;
using System.Windows.Forms;
using ACBrLib;
using ACBrLib.Core;
using ACBrLib.Core.DFe;
using ACBrLib.Core.Extensions;
using ACBrLib.Core.NFSe;
using ACBrLib.Core.PosPrinter;
using ACBrLib.NFSe;
using ACBrLib.Core.Config;
using System.Linq;

namespace ACBrLibNFSe.Demo
{
    public partial class FrmMain : Form
    {
        #region Fields

        private ACBrNFSe ACBrNFSe;

        #endregion Fields

        #region Constructors

        public FrmMain()
        {
            InitializeComponent();

            // Inicializando a classe e carregando a dll
            ACBrNFSe = new ACBrNFSe();
        }

        #endregion Constructors

        #region Methods

        #region EventHandlers

        private void FrmMain_Shown(object sender, EventArgs e)
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Carregando...");

            

            try
            {
                cmbCidadeEmitente.EnumDataSource(CodigoMunicipio.NenhumaCidadeSelecionada);
                cmbLayoutNFSe.EnumDataSource(LayoutNFSe.lnfsProvedor);
                cmbFormaEmissao.EnumDataSource(TipoEmissao.teNormal);
                cmbUfDestino.SelectedItem = "SP";
                cmbSSlType.EnumDataSource(SSLType.LT_all);
                cmbCrypt.EnumDataSource(SSLCryptLib.cryWinCrypt);
                cmbHttp.EnumDataSource(SSLHttpLib.httpWinHttp);
                cmbXmlSign.EnumDataSource(SSLXmlSignLib.xsLibXml2);

                // Altera as config de log
                ACBrNFSe.Config.Principal.LogNivel = NivelLog.logParanoico;

                var logPath = Path.Combine(Application.StartupPath, "Logs");
                if (!Directory.Exists(logPath))
                    Directory.CreateDirectory(logPath);

                ACBrNFSe.Config.Principal.LogPath = logPath;
                ACBrNFSe.ConfigGravar();

                LoadConfig();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            // Liberando a dll
            ACBrNFSe.Dispose();
        }

        #endregion EventHandlers

        #endregion Methods

        private void btnSalvarConfiguracoes_Click(object sender, EventArgs e)
        {
            try
            {
                SalvarConfig();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void SalvarConfig()
        {
            SplashScreenManager.Show<FrmWait>();
            SplashScreenManager.ShowInfo(SplashInfo.Message, "Salvando...");

            try
            {
                //Config Geral
                ACBrNFSe.Config.ExibirErroSchema = ckbExibirErroSchema.Checked;
                ACBrNFSe.Config.FormatoAlerta = txtFormatoAlerta.Text;
                ACBrNFSe.Config.FormaEmissao = cmbFormaEmissao.GetSelectedValue<TipoEmissao>();
                ACBrNFSe.Config.RetirarAcentos = ckbRetirarAcentos.Checked;
                ACBrNFSe.Config.SalvarGer = ckbSalvarArqeResp.Checked;
                ACBrNFSe.Config.PathSalvar = txtArqNFSe.Text;
                ACBrNFSe.Config.MontarPathSchema = ckbMontarPathSchemas.Checked;
                ACBrNFSe.Config.PathSchemas = txtSchemaPath.Text;
                ACBrNFSe.Config.ConsultaLoteAposEnvio = ckbConsultarLoteAposEnvio.Checked;
                ACBrNFSe.Config.ConsultaAposCancelar = ckbConsultarAposCancelar.Checked;
                ACBrNFSe.Config.LayoutNFSe = cmbLayoutNFSe.GetSelectedValue<LayoutNFSe>();

                //Config WebService
                ACBrNFSe.Config.DFe.UF = cmbUfDestino.Text;
                ACBrNFSe.Config.SSLType = cmbSSlType.GetSelectedValue<SSLType>();
                ACBrNFSe.Config.Timeout = (int)nudTimeOut.Value;
                ACBrNFSe.Config.Ambiente = rdbHomologacao.Checked ? TipoAmbiente.taHomologacao : TipoAmbiente.taProducao;
                ACBrNFSe.Config.Visualizar = ckbVisualizar.Checked;
                ACBrNFSe.Config.SalvarWS = ckbSalvarSOAP.Checked;

                //Proxy
                ACBrNFSe.Config.Proxy.Servidor = txtProxyServidor.Text;
                ACBrNFSe.Config.Proxy.Porta = nudProxyPorta.Text;
                ACBrNFSe.Config.Proxy.Usuario = txtProxyUsuario.Text;
                ACBrNFSe.Config.Proxy.Senha = txtProxySenha.Text;

                //Certificado
                ACBrNFSe.Config.DFe.SSLCryptLib = cmbCrypt.GetSelectedValue<SSLCryptLib>();
                ACBrNFSe.Config.DFe.SSLHttpLib = cmbHttp.GetSelectedValue<SSLHttpLib>();
                ACBrNFSe.Config.DFe.SSLXmlSignLib = cmbXmlSign.GetSelectedValue<SSLXmlSignLib>();
                ACBrNFSe.Config.DFe.ArquivoPFX = txtCertPath.Text;
                ACBrNFSe.Config.DFe.Senha = txtCertPassword.Text;
                ACBrNFSe.Config.DFe.NumeroSerie = txtCertNumero.Text;
                ACBrNFSe.Config.DFe.DadosPFX = txtDadosPFX.Text;

                //Arquivos
                ACBrNFSe.Config.SalvarArq = ckbSalvarArqs.Checked;
                ACBrNFSe.Config.SepararPorMes = ckbPastaMensal.Checked;
                ACBrNFSe.Config.AdicionarLiteral = ckbAdicionarLiteralNomePastas.Checked;
                ACBrNFSe.Config.EmissaoPathNFSe = ckbEmissaoPathNFSe.Checked;
                ACBrNFSe.Config.SepararPorCNPJ = ckbSepararPorCNPJ.Checked;

                //Documento Auxiliar
                ACBrNFSe.Config.DANFSe.PathLogo = txtLogomarcaPrefeitura.Text;
                ACBrNFSe.Config.DANFSe.Prestador.Logo = txtLogoMarcaPrestadorServico.Text;
                ACBrNFSe.Config.DANFSe.Prefeitura = txtNomePrefeitura.Text;
                ACBrNFSe.Config.DANFSe.PathPDF = txtPastaPDF.Text;

                //Email
                ACBrNFSe.Config.Email.Nome = txtNome.Text;
                ACBrNFSe.Config.Email.Conta = txtEmail.Text;
                ACBrNFSe.Config.Email.Usuario = txtUsuario.Text;
                ACBrNFSe.Config.Email.Senha = txtSenha.Text;
                ACBrNFSe.Config.Email.Servidor = txtHost.Text;
                ACBrNFSe.Config.Email.Porta = nudPorta.Text;
                ACBrNFSe.Config.Email.SSL = ckbSSL.Checked;
                ACBrNFSe.Config.Email.TLS = ckbTLS.Checked;

                //Emitente
                ACBrNFSe.Config.Emitente.CNPJ = txtCNPJEmitente.Text;
                ACBrNFSe.Config.Emitente.InscMun = txtInscMunicipalEmitente.Text;
                ACBrNFSe.Config.Emitente.RazSocial = txtRazaoSocialEmitente.Text;
                ACBrNFSe.Config.Emitente.WSUser = txtEmitenteUsuário.Text;
                ACBrNFSe.Config.Emitente.WSSenha = txtEmitenteSenha.Text;
                ACBrNFSe.Config.Emitente.WSFraseSecr = txtEmitenteFraseSecreta.Text;
                ACBrNFSe.Config.Emitente.WSChaveAcesso = txtEmitenteChaveAcesso.Text;
                ACBrNFSe.Config.Emitente.WSChaveAutoriz = txtEmitenteChaveAutorizacao.Text;
                ACBrNFSe.Config.Emitente.Dados.NomeFantasia = txtFantasiaEmitente.Text;
                ACBrNFSe.Config.Emitente.Dados.Telefone = txtFoneEmitente.Text;
                ACBrNFSe.Config.Emitente.Dados.CEP = txtCEPEmitente.Text;
                ACBrNFSe.Config.Emitente.Dados.Endereco = txtLogradouroEmitente.Text;
                ACBrNFSe.Config.Emitente.Dados.Numero = txtNumeroEmitente.Text;
                ACBrNFSe.Config.Emitente.Dados.Complemento = txtComplementoEmitente.Text;
                ACBrNFSe.Config.Emitente.Dados.Bairro = txtBairroEmitente.Text;
                ACBrNFSe.Config.CodigoMunicipio = cmbCidadeEmitente.GetSelectedValue<CodigoMunicipio>();
                txtCodCidadeEmitente.Text = ACBrNFSe.Config.CodigoMunicipio.GetEnumValueOrInt<CodigoMunicipio>();

                ACBrNFSe.ConfigGravar();

                Application.DoEvents();
            }
            finally
            {
                SplashScreenManager.Close();
            }
        }

        private void btnCarregarConfiguracoes_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos Ini (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            LoadConfig(file);
        }

        private void LoadConfig(string file = "ACBrLib.ini")
        {
            ACBrNFSe.ConfigLer(file);

            //Config Geral
            ckbExibirErroSchema.Checked = ACBrNFSe.Config.ExibirErroSchema;
            txtFormatoAlerta.Text = ACBrNFSe.Config.FormatoAlerta;
            cmbFormaEmissao.SetSelectedValue(ACBrNFSe.Config.FormaEmissao);
            ckbRetirarAcentos.Checked = ACBrNFSe.Config.RetirarAcentos;
            ckbSalvarArqeResp.Checked = ACBrNFSe.Config.SalvarGer;
            txtArqNFSe.Text = ACBrNFSe.Config.PathSalvar;
            ckbMontarPathSchemas.Checked = ACBrNFSe.Config.MontarPathSchema;
            txtSchemaPath.Text = ACBrNFSe.Config.PathSchemas;
            ckbConsultarLoteAposEnvio.Checked = ACBrNFSe.Config.ConsultaLoteAposEnvio;
            ckbConsultarAposCancelar.Checked = ACBrNFSe.Config.ConsultaAposCancelar;
            cmbLayoutNFSe.SetSelectedValue(ACBrNFSe.Config.LayoutNFSe);

            //Config Webservice
            cmbUfDestino.SelectedItem = ACBrNFSe.Config.DFe.UF;
            cmbSSlType.SetSelectedValue(ACBrNFSe.Config.SSLType);
            nudTimeOut.Value = ACBrNFSe.Config.Timeout;

            var ambiente = ACBrNFSe.Config.Ambiente;
            rdbHomologacao.Checked = ambiente == TipoAmbiente.taHomologacao;
            rdbProducao.Checked = ambiente == TipoAmbiente.taProducao;

            ckbVisualizar.Checked = ACBrNFSe.Config.Visualizar;
            ckbSalvarSOAP.Checked = ACBrNFSe.Config.SalvarWS;
            ckbAjustarAut.Checked = ACBrNFSe.Config.AjustaAguardaConsultaRet;
            nudAguardar.Value = ACBrNFSe.Config.AguardarConsultaRet;
            nudTentativas.Value = ACBrNFSe.Config.Tentativas;
            nudIntervalos.Value = ACBrNFSe.Config.IntervaloTentativas;

            //Proxy WebService
            txtProxyServidor.Text = ACBrNFSe.Config.Proxy.Servidor;
            nudProxyPorta.Text = ACBrNFSe.Config.Proxy.Porta;
            txtProxyUsuario.Text = ACBrNFSe.Config.Proxy.Usuario;
            txtProxySenha.Text = ACBrNFSe.Config.Proxy.Senha;

            //Config Certificado
            cmbCrypt.SetSelectedValue(ACBrNFSe.Config.DFe.SSLCryptLib);
            cmbHttp.SetSelectedValue(ACBrNFSe.Config.DFe.SSLHttpLib);
            cmbXmlSign.SetSelectedValue(ACBrNFSe.Config.DFe.SSLXmlSignLib);
            txtCertPath.Text = ACBrNFSe.Config.DFe.ArquivoPFX;
            txtCertPassword.Text = ACBrNFSe.Config.DFe.Senha;
            txtCertNumero.Text = ACBrNFSe.Config.DFe.NumeroSerie;
            txtDadosPFX.Text = ACBrNFSe.Config.DFe.DadosPFX;

            //Config Arquivos
            ckbSalvarArqs.Checked = ACBrNFSe.Config.SalvarArq;
            ckbPastaMensal.Checked = ACBrNFSe.Config.SepararPorMes;
            ckbAdicionarLiteralNomePastas.Checked = ACBrNFSe.Config.AdicionarLiteral;
            ckbEmissaoPathNFSe.Checked = ACBrNFSe.Config.EmissaoPathNFSe;
            ckbSepararPorCNPJ.Checked = ACBrNFSe.Config.SepararPorCNPJ;

            //Config Documento Auxiliar
            txtLogomarcaPrefeitura.Text = ACBrNFSe.Config.DANFSe.PathLogo;
            txtLogoMarcaPrestadorServico.Text = ACBrNFSe.Config.DANFSe.Prestador.Logo;
            txtNomePrefeitura.Text = ACBrNFSe.Config.DANFSe.Prefeitura;
            txtPastaPDF.Text = ACBrNFSe.Config.DANFSe.PathPDF;

            //Config Email
            txtNome.Text = ACBrNFSe.Config.Email.Nome;
            txtEmail.Text = ACBrNFSe.Config.Email.Conta;
            txtUsuario.Text = ACBrNFSe.Config.Email.Usuario;
            txtSenha.Text = ACBrNFSe.Config.Email.Senha;
            txtHost.Text = ACBrNFSe.Config.Email.Servidor;
            nudPorta.Text = ACBrNFSe.Config.Email.Porta;
            ckbSSL.Checked = ACBrNFSe.Config.Email.SSL;
            ckbTLS.Checked = ACBrNFSe.Config.Email.TLS;

            //Config Emitente
            txtCNPJEmitente.Text = ACBrNFSe.Config.Emitente.CNPJ;
            txtInscMunicipalEmitente.Text = ACBrNFSe.Config.Emitente.InscMun;
            txtRazaoSocialEmitente.Text = ACBrNFSe.Config.Emitente.RazSocial;
            txtEmitenteUsuário.Text = ACBrNFSe.Config.Emitente.WSUser;
            txtEmitenteSenha.Text = ACBrNFSe.Config.Emitente.WSSenha;
            txtEmitenteFraseSecreta.Text = ACBrNFSe.Config.Emitente.WSFraseSecr;
            txtEmitenteChaveAcesso.Text = ACBrNFSe.Config.Emitente.WSChaveAcesso;
            txtEmitenteChaveAutorizacao.Text = ACBrNFSe.Config.Emitente.WSChaveAutoriz;
            txtFantasiaEmitente.Text = ACBrNFSe.Config.Emitente.Dados.NomeFantasia;
            txtFoneEmitente.Text = ACBrNFSe.Config.Emitente.Dados.Telefone;
            txtCEPEmitente.Text = ACBrNFSe.Config.Emitente.Dados.CEP;
            txtLogradouroEmitente.Text = ACBrNFSe.Config.Emitente.Dados.Endereco;
            txtNumeroEmitente.Text = ACBrNFSe.Config.Emitente.Dados.Numero;
            txtComplementoEmitente.Text = ACBrNFSe.Config.Emitente.Dados.Complemento;
            txtBairroEmitente.Text = ACBrNFSe.Config.Emitente.Dados.Bairro;
            cmbCidadeEmitente.SetSelectedValue(ACBrNFSe.Config.CodigoMunicipio);
            txtCodCidadeEmitente.Text = ACBrNFSe.Config.CodigoMunicipio.GetEnumValueOrInt<CodigoMunicipio>();
        }

        private void CheckNFSeLista(bool xml = false)
        {
            if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibNFSe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                ACBrNFSe.LimparLista();

            if (xml)
                CarregarNFSeXml();
            else
                CarregarNFSeIni();
        }

        private void CarregarNFSeXml()
        {
            var arquivoXML = Helpers.OpenFile("Arquivo Xml NFSe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoXML)) return;

            ACBrNFSe.CarregarXML(arquivoXML);
        }

        private void CarregarNFSeIni()
        {
            var arquivoINI = Helpers.OpenFile("Arquivo Ini NFSe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
            if (string.IsNullOrEmpty(arquivoINI)) return;

            ACBrNFSe.CarregarINI(arquivoINI);
        }

        private void btnLimparListaNFSe_Click(object sender, EventArgs e)
        {
            try
            {
                if (MessageBox.Show(@"Limpar a lista ?", @"ACBrLibNFSe", MessageBoxButtons.YesNo) == DialogResult.Yes)
                    ACBrNFSe.LimparLista();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSelectLog_Click(object sender, EventArgs e)
        {
            txtLogs.Text = Helpers.SaveFile("Arquivos Logs (*.log)|*.log|Todos os Arquivos (*.*)|*.*");
        }

        private void btnSelectSchema_Click(object sender, EventArgs e)
        {
            txtSchemaPath.Text = Helpers.SelectFolder();
        }

        private void btnSelecionarCertificado_Click(object sender, EventArgs e)
        {
            txtCertPath.Text = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
        }

        private void btnDadosPFX_Click(object sender, EventArgs e)
        {
            var file = Helpers.OpenFile("Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*");
            if (!File.Exists(file)) return;

            var dados = File.ReadAllBytes(file);
            txtDadosPFX.Text = Convert.ToBase64String(dados);
        }

        private void btnArqNFSe_Click(object sender, EventArgs e)
        {
            txtArqNFSe.Text = Helpers.SelectFolder();
        }

        private void btnObterCertificados_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrNFSe.ObterCertificados();
                rtbRespostas.AppendLine(ret.Select(x => x.ToString()).ToArray());
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEmitirNota_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFSeLista();

                var aLote = "1";
                if (InputBox.Show("Emitir", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFSe.Emitir(aLote, 0, false);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarEnviarLoteAssincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFSeLista();

                var aLote = "1";
                if (InputBox.Show("Enviar Assíncrono", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFSe.Emitir(aLote, 2, false);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarEnviarLoteSincrono_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFSeLista();

                var aLote = "1";
                if (InputBox.Show("Enviar Síncrono", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFSe.Emitir(aLote, 3, false);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarRPS_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFSeLista();

                var aLote = "1";
                if (InputBox.Show("WebServices Enviar", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFSe.Emitir(aLote, 4, false);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEmail_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXml = Helpers.OpenFile("Arquivo Xmnl NFe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXml)) return;

                var destinatario = "";
                if (InputBox.Show("Envio email", "Digite o email do destinatario", ref destinatario) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(destinatario)) return;

                ACBrNFSe.EnviarEmail(destinatario, arquivoXml, true, txtAssunto.Text, "", "", txtMensagem.Text);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnSubstituirNFSe_Click(object sender, EventArgs e)
        {
            try
            {
                var numeroNFSe = "";
                if (InputBox.Show("Substituir NFSe", "Informe o Numero da NFSe", ref numeroNFSe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(numeroNFSe)) return;

                var serieNFSe = "";
                if (InputBox.Show("Substituir NFSe", "Informe a série da NFSe", ref serieNFSe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(serieNFSe)) return;

                var codCancelamento = "";
                if (InputBox.Show("Substituir NFSe", "Informe o Código de Cancelamento", ref codCancelamento) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(codCancelamento)) return;

                var motivoCancelamento = "";
                if (InputBox.Show("Substituir NFSe", "Informe o Motivo do Cancelamento", ref motivoCancelamento) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(motivoCancelamento)) return;

                var numeroLote = "";
                if (InputBox.Show("Substituir NFSe", "Informe o Numero do Lote", ref numeroLote) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(numeroLote)) return;

                var codVerificacao = "";
                if (InputBox.Show("Substituir NFSe", "Informe o Código de Verificação", ref codVerificacao) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(codVerificacao)) return;

                var ret = ACBrNFSe.SubstituirNFSe(numeroNFSe, serieNFSe, codCancelamento, motivoCancelamento, numeroLote, codVerificacao);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirDANFSe_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXML = Helpers.OpenFile("Arquivo Xml NFSe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXML)) return;

                ACBrNFSe.LimparLista();
                ACBrNFSe.CarregarXML(arquivoXML);
                ACBrNFSe.ImprimirPDF();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnImprimirNFSe_Click(object sender, EventArgs e)
        {
            try
            {
                var arquivoXML = Helpers.OpenFile("Arquivo Xml NFSe (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoXML)) return;

                ACBrNFSe.LimparLista();
                ACBrNFSe.CarregarXML(arquivoXML);
                ACBrNFSe.Imprimir(bMostrarPreview: true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnLinkNFSe_Click(object sender, EventArgs e)
        {
            try
            {
                var numeroNFSe = "";
                if (InputBox.Show("Link NFSe", "Informe o Numero da NFSe", ref numeroNFSe) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(numeroNFSe)) return;

                var codVerificacao = "";
                if (InputBox.Show("Link NFSe", "Informe o Código de Verificação", ref codVerificacao) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(codVerificacao)) return;

                var chaveAcesso = "";
                if (InputBox.Show("Link NFSe", "Informe a Chave de Acesso", ref chaveAcesso) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(chaveAcesso)) return;

                var valorServico = "";
                if (InputBox.Show("Link NFSe", "Informe o Valor do Serviço", ref valorServico) != DialogResult.OK) return;
                if (string.IsNullOrEmpty(valorServico)) return;

                var ret = ACBrNFSe.LinkNFSe(numeroNFSe, codVerificacao, chaveAcesso, valorServico);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarLoteRPS_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFSeLista();

                var aLote = "1";
                if (InputBox.Show("Gerar Lote RPS", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var qtdNaximaRPS = 1;
                if (InputBox.Show("Gerar Lote RPS", "Quantidade Máxima RPS", ref qtdNaximaRPS) != DialogResult.OK) return;

                var ret = ACBrNFSe.GerarLote(aLote, qtdNaximaRPS, 0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGerarToken_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrNFSe.GerarToken();
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCarregarXMLNFse_Click(object sender, EventArgs e)
        {
            try
            {
                CheckNFSeLista(true);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnObterXMLNFSe_Click(object sender, EventArgs e)
        {
            try
            {
                var ret = ACBrNFSe.ObterXml(0);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnGravarXMLNFSe_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrNFSe.GravarXml(0);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarSituacaoLote_Click(object sender, EventArgs e)
        {
            try
            {
                var protocolo = "";
                if (InputBox.Show("Consultar Situação", "Número do Protocolo", ref protocolo) != DialogResult.OK) return;

                var aLote = "";
                if (InputBox.Show("Consultar Situação", "Número do Lote", ref aLote) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarSitucao(protocolo, aLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSePorNumero_Click(object sender, EventArgs e)
        {
            try
            {
                var protocolo = "";
                if (InputBox.Show("Consultar Por Numero", "Número do Protocolo", ref protocolo) != DialogResult.OK) return;

                var pagina = 0;
                if (InputBox.Show("Consultar Por Numero", "Informe a Pagina", ref pagina) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSePorNumero(protocolo, pagina);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeGenerico_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrNFSe.LimparLista();

                var arquivoINI = Helpers.OpenFile("Arquivo Ini NFSe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoINI)) return;

                ACBrNFSe.CarregarINI(arquivoINI);

                var ret = ACBrNFSe.ConsultarNFSeGenerico(arquivoINI);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarLoteRPS_Click(object sender, EventArgs e)
        {
            try
            {
                var protocolo = "";
                if (InputBox.Show("Consultar Lote Por RPS", "Número do Protocolo", ref protocolo) != DialogResult.OK) return;

                var numLote = "";
                if (InputBox.Show("Consultar Lote Por RPS", "Número do Lote", ref numLote) != DialogResult.OK) return;


                var ret = ACBrNFSe.ConsultarLoteRps(protocolo, numLote);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSePorPeriodo_Click(object sender, EventArgs e)
        {
            try
            {
                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Por Periodo", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Por Periodo", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Por Periodo", "Informe a Pagina", ref pagina) != DialogResult.OK) return;

                var numeroLote = "";
                if (InputBox.Show("Consultar NFSe Por Periodo", "Informe Numero do Lote", ref numeroLote) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Por Periodo", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSePorPeriodo(DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), pagina, numeroLote, tipoPeriodo);
                rtbRespostas.AppendLine(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSePorRPS_Click(object sender, EventArgs e)
        {
            try
            {
                var numeroRps = "";
                if (InputBox.Show("Consultar NFSe Por RPS", "Número do Protocolo", ref numeroRps) != DialogResult.OK) return;

                var serie = "";
                if (InputBox.Show("Consultar NFSe Por RPS", "Número do Lote", ref serie) != DialogResult.OK) return;

                var tipo = "";
                if (InputBox.Show("Consultar NFSe Por RPS", "Tipo", ref tipo) != DialogResult.OK) return;

                var codVerificacao = "";
                if (InputBox.Show("Consultar NFSe Por RPS", "Código de Verificação", ref codVerificacao) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSePorRps(numeroRps, serie, tipo, codVerificacao);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSePorFaixa_Click(object sender, EventArgs e)
        {
            try
            {
                var numeroInicial = "";
                if (InputBox.Show("Consultar NFSe Por Faixa", "Informe o Número Inicial", ref numeroInicial) != DialogResult.OK) return;

                var numeroFinal = "";
                if (InputBox.Show("Consultar NFSe Por Faixa", "Informe o Número Final", ref numeroFinal) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Por Faixa", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSePorFaixa(numeroInicial, numeroFinal, pagina);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoPrestadoPorNumero_Click(object sender, EventArgs e)
        {
            try
            {
                var numero = "";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Numero", "Informe o Número", ref numero) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Numero", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Numero", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Numero", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Numero", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoPrestadoPorNumero(numero, pagina, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeSevicoPrestadoPorTomador_Click(object sender, EventArgs e)
        {
            try
            {
                var cnpj = "";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Tomador", "Informe o CNPJ", ref cnpj) != DialogResult.OK) return;

                var inscMun = "";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Tomador", "Informe a Inscrição Municipal", ref inscMun) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Tomador", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Tomador", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Tomador", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Tomador", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoPrestadoPorTomador(cnpj, inscMun, pagina, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoPrestadoPorPeriodo_Click(object sender, EventArgs e)
        {
            try
            {
                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Periodo", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Periodo", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Periodo", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Periodo", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoPrestadoPorPeriodo(DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), pagina, tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoPrestadoPorIntermediario_Click(object sender, EventArgs e)
        {
            try
            {
                var cnpj = "";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Intermediário", "Informe o CNPJ", ref cnpj) != DialogResult.OK) return;

                var inscMun = "";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Intermediário", "Informe a Inscrição Municipal", ref inscMun) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Intermediário", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Intermediário", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Intermediário", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Prestado Por Intermediário", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoPrestadoPorIntermediario(cnpj, inscMun, pagina, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoTomadoPorNumero_Click(object sender, EventArgs e)
        {
            try
            {
                var numero = "";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Numero", "Informe o Número", ref numero) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Numero", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Numero", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Numero", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Numero", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoTomadoPorNumero(numero, pagina,DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), tipoPeriodo);
                rtbRespostas.AppendText(ret);
            } 
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoTomadoPorPrestador_Click(object sender, EventArgs e)
        {
            try
            {
                var cnpj = "";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Prestador", "Informe o CNPJ", ref cnpj) != DialogResult.OK) return;

                var inscMun = "";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Prestador", "Informe a Inscrição Municipal", ref inscMun) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Prestador", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Prestador", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Prestador", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Prestador", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoTomadoPorPrestador(cnpj, inscMun, pagina, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoTomadoPorTomador_Click(object sender, EventArgs e)
        {
            try
            {
                var cnpj = "";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Tomador", "Informe o CNPJ", ref cnpj) != DialogResult.OK) return;

                var inscMun = "";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Tomador", "Informe a Inscrição Municipal", ref inscMun) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Tomador", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Tomador", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Tomador", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Tomador", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoTomadoPorTomador(cnpj, inscMun, pagina, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoTomadoPorPeriodo_Click(object sender, EventArgs e)
        {
            try
            {
                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe a Página", ref pagina) != DialogResult.OK) return;
                
                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoTomadoPorPeriodo(DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), pagina, tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSeServicoTomadoPorIntermediario_Click(object sender, EventArgs e)
        {
            try
            {
                var cnpj = "";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Intermediário", "Informe o CNPJ", ref cnpj) != DialogResult.OK) return;

                var inscMun = "";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Intermediário", "Informe a Inscrição Municipal", ref inscMun) != DialogResult.OK) return;

                var pagina = 1;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe a Página", ref pagina) != DialogResult.OK) return;

                var dataInicial = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe a Data Inicial", ref dataInicial) != DialogResult.OK) return;

                var dataFinal = "dd/MM/yyyy";
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe a Data Final", ref dataFinal) != DialogResult.OK) return;

                var tipoPeriodo = 0;
                if (InputBox.Show("Consultar NFSe Serviço Tomado Por Periodo", "Informe Tipo Periodo", ref tipoPeriodo) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSeServicoTomadoPorIntermediario(cnpj, inscMun, pagina, DateTime.Parse(dataInicial), DateTime.Parse(dataFinal), tipoPeriodo);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnCancelarNFSe_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrNFSe.LimparLista();

                var arquivoINI = Helpers.OpenFile("Arquivo Ini NFSe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoINI)) return;

                ACBrNFSe.CarregarINI(arquivoINI);

                var ret = ACBrNFSe.Cancelar(arquivoINI);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnEnviarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                ACBrNFSe.LimparLista();

                var arquivoINI = Helpers.OpenFile("Arquivo Ini NFSe (*.ini)|*.ini|Todos os Arquivos (*.*)|*.*");
                if (string.IsNullOrEmpty(arquivoINI)) return;

                ACBrNFSe.CarregarINI(arquivoINI);

                var ret = ACBrNFSe.EnviarEvento(arquivoINI);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarDPSPorChave_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveDPS = "";
                if (InputBox.Show("Consultar DPS Por Chave", "Informe a Chave DPS", ref chaveDPS) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarDPSPorChave(chaveDPS);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarNFSePorChave_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveNFSe = "";
                if (InputBox.Show("Consultar NFSe Por Chave", "Informe a Chave NFSe", ref chaveNFSe) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarNFSePorChave(chaveNFSe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarEvento_Click(object sender, EventArgs e)
        {
            try
            {
                var chave = "";
                if (InputBox.Show("Consultar Evento", "Informe a Chave", ref chave) != DialogResult.OK) return;

                var tipoEvento = 0;
                if (InputBox.Show("Consultar Evento", "Informe o Tipo do Evento", ref tipoEvento) != DialogResult.OK) return;

                var numeroSequencia = 0;
                if (InputBox.Show("Consultar Evento", "Informe o Número Sequencia", ref numeroSequencia) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarEvento(chave, tipoEvento, numeroSequencia);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarDFe_Click(object sender, EventArgs e)
        {
            try
            {
                var nsu = 0;
                if (InputBox.Show("Consultar DFe", "Informe o NSU", ref nsu) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarDFe(nsu);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnObterDANFSe_Click(object sender, EventArgs e)
        {
            try
            {
                var chaveNFSe = "";
                if (InputBox.Show("Obter DANFSe", "Informe a Chave NFSe", ref chaveNFSe) != DialogResult.OK) return;

                var ret = ACBrNFSe.ObterDANFSE(chaveNFSe);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }

        private void btnConsultarParametros_Click(object sender, EventArgs e)
        {
            try
            {
                var tipoParametroMunicipio = 0;
                if (InputBox.Show("Consultar Parametros", "Informe o Parametro do Municipio", ref tipoParametroMunicipio) != DialogResult.OK) return;

                var codigoServico = "";
                if (InputBox.Show("Consultar Parametros", "Informe o Código de Serviço", ref codigoServico) != DialogResult.OK) return;

                var competencia = "dd/MM/yyyy";
                if (InputBox.Show("Consultar Parametros", "Informe o Código de Serviço", ref competencia) != DialogResult.OK) return;

                var numeroBeneficio = "";
                if (InputBox.Show("Consultar Parametros", "Informe o Código de Serviço", ref numeroBeneficio) != DialogResult.OK) return;

                var ret = ACBrNFSe.ConsultarParametros(tipoParametroMunicipio, codigoServico, DateTime.Parse(competencia), numeroBeneficio);
                rtbRespostas.AppendText(ret);
            }
            catch (Exception exception)
            {
                MessageBox.Show(exception.Message, @"Erro", MessageBoxButtons.OK, MessageBoxIcon.Error);
            }
        }
    }
}