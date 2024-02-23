{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit Frm_ACBrCIOT;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, OleCtrls, SHDocVw,
  ShellAPI, XMLIntf, XMLDoc, zlib,
  ACBrBase, ACBrDFe, ACBrMail, ACBrCIOT;

type
  TfrmACBrCIOT = class(TForm)
    pnlMenus: TPanel;
    pnlCentral: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    PageControl4: TPageControl;
    TabSheet3: TTabSheet;
    lSSLLib: TLabel;
    lCryptLib: TLabel;
    lHttpLib: TLabel;
    lXmlSign: TLabel;
    gbCertificado: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    sbtnCaminhoCert: TSpeedButton;
    Label25: TLabel;
    sbtnGetCert: TSpeedButton;
    sbtnNumSerie: TSpeedButton;
    edtCaminho: TEdit;
    edtSenha: TEdit;
    edtNumSerie: TEdit;
    btnDataValidade: TButton;
    btnNumSerie: TButton;
    btnSubName: TButton;
    btnCNPJ: TButton;
    btnIssuerName: TButton;
    GroupBox1: TGroupBox;
    Edit1: TEdit;
    btnSha256: TButton;
    cbAssinar: TCheckBox;
    btnLeituraX509: TButton;
    cbSSLLib: TComboBox;
    cbCryptLib: TComboBox;
    cbHttpLib: TComboBox;
    cbXmlSignLib: TComboBox;
    TabSheet4: TTabSheet;
    GroupBox3: TGroupBox;
    sbtnPathSalvar: TSpeedButton;
    Label29: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label42: TLabel;
    spPathSchemas: TSpeedButton;
    edtPathLogs: TEdit;
    ckSalvar: TCheckBox;
    cbFormaEmissao: TComboBox;
    cbxAtualizarXML: TCheckBox;
    cbxExibirErroSchema: TCheckBox;
    edtFormatoAlerta: TEdit;
    cbxRetirarAcentos: TCheckBox;
    cbVersaoDF: TComboBox;
    edtPathSchemas: TEdit;
    TabSheet7: TTabSheet;
    GroupBox4: TGroupBox;
    Label6: TLabel;
    lTimeOut: TLabel;
    lSSLLib1: TLabel;
    cbxVisualizar: TCheckBox;
    cbUF: TComboBox;
    rgTipoAmb: TRadioGroup;
    cbxSalvarSOAP: TCheckBox;
    seTimeOut: TSpinEdit;
    cbSSLType: TComboBox;
    gbProxy: TGroupBox;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    edtProxyHost: TEdit;
    edtProxyPorta: TEdit;
    edtProxyUser: TEdit;
    edtProxySenha: TEdit;
    gbxRetornoEnvio: TGroupBox;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    cbxAjustarAut: TCheckBox;
    edtTentativas: TEdit;
    edtIntervalo: TEdit;
    edtAguardar: TEdit;
    TabSheet12: TTabSheet;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    edtEmitCNPJ: TEdit;
    edtEmitIE: TEdit;
    edtEmitRazao: TEdit;
    edtEmitFantasia: TEdit;
    edtEmitFone: TEdit;
    edtEmitCEP: TEdit;
    edtEmitLogradouro: TEdit;
    edtEmitNumero: TEdit;
    edtEmitComp: TEdit;
    edtEmitBairro: TEdit;
    edtEmitCodCidade: TEdit;
    edtEmitCidade: TEdit;
    edtEmitUF: TEdit;
    TabSheet13: TTabSheet;
    sbPathCIOT: TSpeedButton;
    Label35: TLabel;
    Label47: TLabel;
    sbPathEvento: TSpeedButton;
    cbxSalvarArqs: TCheckBox;
    cbxPastaMensal: TCheckBox;
    cbxAdicionaLiteral: TCheckBox;
    cbxEmissaoPathCIOT: TCheckBox;
    cbxSalvaPathEvento: TCheckBox;
    cbxSepararPorCNPJ: TCheckBox;
    edtPathCIOT: TEdit;
    edtPathEvento: TEdit;
    cbxSepararPorModelo: TCheckBox;
    TabSheet14: TTabSheet;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    edtSmtpHost: TEdit;
    edtSmtpPort: TEdit;
    edtSmtpUser: TEdit;
    edtSmtpPass: TEdit;
    edtEmailAssunto: TEdit;
    cbEmailSSL: TCheckBox;
    mmEmailMsg: TMemo;
    btnSalvarConfig: TBitBtn;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    pgcBotoes: TPageControl;
    tsEnvios: TTabSheet;
    pgRespostas: TPageControl;
    TabSheet5: TTabSheet;
    MemoResp: TMemo;
    TabSheet6: TTabSheet;
    WBResposta: TWebBrowser;
    TabSheet8: TTabSheet;
    memoLog: TMemo;
    TabSheet9: TTabSheet;
    trvwDocumento: TTreeView;
    TabSheet10: TTabSheet;
    memoRespWS: TMemo;
    Dados: TTabSheet;
    MemoDados: TMemo;
    ACBrMail1: TACBrMail;
    OpenDialog1: TOpenDialog;
    ACBrCIOT1: TACBrCIOT;
    Label7: TLabel;
    cbbIntegradora: TComboBox;
    Label30: TLabel;
    edtUsuarioWebService: TEdit;
    Label33: TLabel;
    edtSenhaWebService: TEdit;
    Label34: TLabel;
    edtHashIntegrador: TEdit;
    btnGerarCIOT: TButton;
    btnCriarEnviar: TButton;
    btnEnviarCiotEmail: TButton;
    tsOperacao: TTabSheet;
    rgOperacao: TRadioGroup;
    Button1: TButton;
    Button2: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure sbPathCIOTClick(Sender: TObject);
    procedure sbPathEventoClick(Sender: TObject);
    procedure sbtnCaminhoCertClick(Sender: TObject);
    procedure sbtnNumSerieClick(Sender: TObject);
    procedure sbtnGetCertClick(Sender: TObject);
    procedure btnDataValidadeClick(Sender: TObject);
    procedure btnNumSerieClick(Sender: TObject);
    procedure btnSubNameClick(Sender: TObject);
    procedure btnCNPJClick(Sender: TObject);
    procedure btnIssuerNameClick(Sender: TObject);
    procedure btnSha256Click(Sender: TObject);
    procedure btnLeituraX509Click(Sender: TObject);
    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure spPathSchemasClick(Sender: TObject);
    procedure PathClick(Sender: TObject);
    procedure cbSSLTypeChange(Sender: TObject);
    procedure cbSSLLibChange(Sender: TObject);
    procedure cbCryptLibChange(Sender: TObject);
    procedure cbHttpLibChange(Sender: TObject);
    procedure cbXmlSignLibChange(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure ACBrCIOT1GerarLog(const ALogLine: string; var Tratado: Boolean);
    procedure ACBrCIOT1StatusChange(Sender: TObject);
    procedure btnGerarCIOTClick(Sender: TObject);
    procedure btnCriarEnviarClick(Sender: TObject);
    procedure btnEnviarCiotEmailClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    sToken: string;

    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfigurarComponente;
    procedure ConfigurarEmail;
    Procedure AlimentarComponente;
    procedure LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
    procedure AtualizarSSLLibsCombo;
  public
    { Public declarations }
  end;

var
  frmACBrCIOT: TfrmACBrCIOT;

implementation

uses
  strutils, math, TypInfo, DateUtils, synacode, blcksock, FileCtrl, Grids,
  IniFiles, Printers,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO,
  ACBrUtil.XMLHTML,
  pcnConversao,
  ACBrCIOTConversao,
  ACBrDFeSSL, ACBrDFeOpenSSL, ACBrDFeUtil,
  Frm_Status, Frm_SelecionarCertificado;

const
  SELDIRHELP = 1000;

{$R *.dfm}

{ TfrmACBrCIOT }

procedure TfrmACBrCIOT.ACBrCIOT1GerarLog(const ALogLine: string;
  var Tratado: Boolean);
begin
  memoLog.Lines.Add(ALogLine);
  Tratado := True;
end;

procedure TfrmACBrCIOT.ACBrCIOT1StatusChange(Sender: TObject);
begin
  case ACBrCIOT1.Status of
    stCIOTIdle:
      begin
        if frmStatus <> nil then
          frmStatus.Hide;
      end;

    stCIOTEnviar:
      begin
        if frmStatus = nil then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando dados do Contrato...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;

    stCIOTRetEnviar:
      begin
        if frmStatus = nil then
          frmStatus := TfrmStatus.Create(Application);

          frmStatus.lblStatus.Caption := 'Recebendo dados do CIOT...';
          frmStatus.Show;
          frmStatus.BringToFront;
      end;

    stCIOTEmail:
      begin
        if frmStatus = nil then
          frmStatus := TfrmStatus.Create(Application);

        frmStatus.lblStatus.Caption := 'Enviando CIOT por e-mail...';
        frmStatus.Show;
        frmStatus.BringToFront;
      end;
  end;

  Application.ProcessMessages;
end;

procedure TfrmACBrCIOT.AlimentarComponente;
begin
  with ACBrCIOT1.Contratos.Add.CIOT do
  begin
    // Só é necessario se usar usuario e senha e não o certificado
    Integradora.Token := sToken;

    case rgOperacao.ItemIndex of
      0: begin
           // Login - Solicita Token
           Integradora.Operacao := opLogin;
         end;

      1: begin
           // Cadastrar Proprietário do Veículo
           Integradora.Operacao := opGravarProprietario;

           with GravarProprietario do
           begin
             CNPJ        := edtEmitCNPJ.Text;
             TipoPessoa  := tpJuridica;
             RazaoSocial := edtEmitRazao.Text;
             RNTRC       := '123456789';

             Endereco.Bairro          := edtEmitBairro.Text;
             Endereco.Rua             := edtEmitLogradouro.Text;
             Endereco.Numero          := edtEmitNumero.Text;
             Endereco.Complemento     := edtEmitComp.Text;
             Endereco.CEP             := edtEmitCEP.Text;
             Endereco.CodigoMunicipio := StrToIntDef(edtEmitCodCidade.Text, 0);

             Telefones.Celular.DDD := 11;
             Telefones.Celular.Numero := StrToIntDef(edtEmitFone.Text, 0);

             Telefones.Fixo.DDD := 0;
             Telefones.Fixo.Numero := 0;

             Telefones.Fax.DDD := 0;
             Telefones.Fax.Numero := 0;
           end;
         end;

      2: begin
           // Cadastrar Veiculo
           Integradora.Operacao := opGravarVeiculo;

           with GravarVeiculo do
           begin
             Placa           := 'ABC-1234';
             Renavam         := '123456789';
             Chassi          := '123456789';
             RNTRC           := '1234';
             NumeroDeEixos   := 2;
             CodigoMunicipio := 3512345;
             Marca           := 'VW';
             Modelo          := 'XYZ';
             AnoFabricacao   := 2010;
             AnoModelo       := 2010;
             Cor             := 'Preto';
             Tara            := 100;
             CapacidadeKg    := 10000;
             CapacidadeM3    := 10000;
             TipoRodado      := trToco;
             TipoCarroceria  := tcFechadaOuBau;
           end;
         end;

      3: begin
           // Cadastrar Motorista
           Integradora.Operacao := opGravarMotorista;

           with GravarMotorista do
           begin
             CPF                 := '12345678901';
             Nome                := 'jose da silva';
             CNH                 := '123456789';
             DataNascimento      := StrToDate('10/10/1970');
             NomeDeSolteiraDaMae := 'joana pereira';

             Endereco.Bairro := 'teste';
             Endereco.Rua := 'teste';
             Endereco.Numero := '200';
             Endereco.Complemento := 'teste';
             Endereco.CEP := '89870000';
             Endereco.CodigoMunicipio := 4212908;

             Telefones.Celular.DDD := 0;
             Telefones.Celular.Numero := 0;

             Telefones.Fixo.DDD := 49;
             Telefones.Fixo.Numero := 33661011;

             Telefones.Fax.DDD := 0;
             Telefones.Fax.Numero := 0;
           end;
         end;

      4: begin
           //Adicionar uma operação de transporte
           Integradora.Operacao := opAdicionar;

           with AdicionarOperacao do
           begin
             (****************  DADOS DO CONTRATO  **************)
             TipoViagem := Padrao; // TAC_Agregado;
             TipoPagamento := eFRETE;
             EmissaoGratuita := (TipoPagamento = TransferenciaBancaria);
             BloquearNaoEquiparado := False;
             MatrizCNPJ := edtEmitCNPJ.text;
             FilialCNPJ := edtEmitCNPJ.text;
             //Id / Chave primária da Tabela do banco de dados do CIOT
             IdOperacaoCliente := '1';
             DataInicioViagem := Now;
             DataFimViagem := Now;
             CodigoNCMNaturezaCarga := 0;
             PesoCarga := 10;
             //utilizado somente para as viagens do tipo Padrão
             TipoEmbalagem := tePallet;

             //Somente para TipoViagem TAC_Agregado
             with Viagens.New do
             begin
               DocumentoViagem := 'CTe';
               CodigoMunicipioOrigem := 4212908; //Pinhalzinho SC
               CodigoMunicipioDestino := 4217303; //Saudades SC
               CepOrigem := '';
               CepDestino := '';
               DistanciaPercorrida := 100;

               Valores.TotalOperacao := 50;
               Valores.TotalViagem := 50;
               Valores.TotalDeAdiantamento := 10;
               Valores.TotalDeQuitacao := 10;
               Valores.Combustivel := 20;
               Valores.Pedagio := 10;
               Valores.OutrosCreditos := 1;
               Valores.JustificativaOutrosCreditos := 'Teste';
               Valores.Seguro := 10;
               Valores.OutrosDebitos := 1;
               Valores.JustificativaOutrosDebitos := 'Teste outros Debitos';

               TipoPagamento := TransferenciaBancaria;

               with NotasFiscais.New do
               begin
                 Numero := '12345';
                 Serie := '1';
                 Data := Date;
                 ValorTotal := 100;

                 ValorDaMercadoriaPorUnidade := 100;
                 CodigoNCMNaturezaCarga := 5501;
                 DescricaoDaMercadoria := 'Produto Teste';
                 UnidadeDeMedidaDaMercadoria := umKg;
                 TipoDeCalculo := SemQuebra;
                 ValorDoFretePorUnidadeDeMercadoria := 0; //Se tiver quebra deve ser informado
                 QuantidadeDaMercadoriaNoEmbarque := 1;

                 ToleranciaDePerdaDeMercadoria.Tipo := tpPorcentagem;
                 ToleranciaDePerdaDeMercadoria.Valor := 2; //Valor da tolerância admitido.

                 DiferencaDeFrete.Tipo := Integral;
                 DiferencaDeFrete.Base := QuantidadeDesembarque;

                 DiferencaDeFrete.Tolerancia.Tipo := tpPorcentagem;
                 DiferencaDeFrete.Tolerancia.Valor := 5; //Valor da tolerância admitido(Nenhum: 0; Porcentagem: 0.00 – 100.00; Absoluto: Livre)

                 DiferencaDeFrete.MargemGanho.Tipo := tpPorcentagem;
                 DiferencaDeFrete.MargemGanho.Valor := 5;

                 DiferencaDeFrete.MargemPerda.Tipo := tpPorcentagem;
                 DiferencaDeFrete.MargemPerda.Valor := 5;
               end;
             end;

             //Não esperado para TipoViagem Frota.
             with Impostos do
             begin
               IRRF := 0;
               SestSenat := 0;
               INSS := 0;
               ISSQN := 0;
               OutrosImpostos := 0;
               DescricaoOutrosImpostos := '';
             end;

             with Pagamentos.New do
             begin
               IdPagamentoCliente := '1';
               DataDeLiberacao := Date;
               Valor := 10;
               TipoPagamento := TransferenciaBancaria; //TransferenciaBancaria(EmissaoGratuita = true); eFRETE (EmissaoGratuita = false)
               Categoria := tcpSemCategoria;//Para os TipoViagem Frota e TAC_Agregado são suportadas as Categorias Frota e SemCategoria. Para o TipoViagem Padrão todas as categorias são suportadas.
               Documento := ''; //Documento relacionado a viagem.

               InformacoesBancarias.InstituicaoBancaria := '756'; //Bancoob
               InformacoesBancarias.Agencia := '';
               InformacoesBancarias.Conta := '';
               InformacoesBancarias.TipoConta := tcContaCorrente;

               InformacaoAdicional := '';
               //CNPJ que deve ser gerada a Nota Fiscal do abastecimento,
               //sendo da mesma raíz do CNPJ da matriz do contratante,
               //apenas aplicável para Categoria Frota (Abastecimento)
               CnpjFilialAbastecimento := AdicionarOperacao.MatrizCNPJ;
             end;

             //TAC ou seu equiparado, que efetuar o transporte rodoviário de cargas por
             //conta de terceiros e mediante remuneração, indicado no cadastramento da Operação de Transporte.
             //Para o TipoViagem Frota o Contratado será a própria empresa que está declarando a operação.
             with Contratado do
             begin
               CpfOuCnpj := '12345678910';
               RNTRC := '12345678';
             end;

             with Motorista do
             begin
               CpfOuCnpj := '12345678910';
               CNH := '12345678910';

               Celular.DDD := 49;
               Celular.Numero := 123456789;
             end;

             //Destinatário da carga.
             //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
             //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
             //Não esperado para TipoViagem Frota.
             with Destinatario do
             begin
               NomeOuRazaoSocial := '';
               CpfOuCnpj := '';

               EMail := '';
               ResponsavelPeloPagamento := False;

               Endereco.Bairro := 'teste';
               Endereco.Rua := '';
               Endereco.Numero := '';
               Endereco.Complemento := '';
               Endereco.CEP := '';
               Endereco.CodigoMunicipio := 0;

               Telefones.Celular.DDD := 0;
               Telefones.Celular.Numero := 0;

               Telefones.Fixo.DDD := 0;
               Telefones.Fixo.Numero := 0;

               Telefones.Fax.DDD := 0;
               Telefones.Fax.Numero := 0;
             end;

             with Contratante do
             begin
               NomeOuRazaoSocial := 'teste';
               CpfOuCnpj := '12345678910';

               EMail := 'teste@teste.com.br';
               ResponsavelPeloPagamento := False;
               RNTRC := '12345678';

               Endereco.Bairro := 'Bela Vista';
               Endereco.Rua := 'Rua Vitória';
               Endereco.Numero := '';
               Endereco.Complemento := '';
               Endereco.CEP := '89870000';
               Endereco.CodigoMunicipio := 4212908;

               Telefones.Celular.DDD := 0;
               Telefones.Celular.Numero := 0;

               Telefones.Fixo.DDD := 49;
               Telefones.Fixo.Numero := 33661012;

               Telefones.Fax.DDD := 0;
               Telefones.Fax.Numero := 0;
             end;

             //É o transportador que contratar outro transportador para realização do
             //transporte de cargas para o qual fora anteriormente contratado,
             //indicado no cadastramento da Operação de Transporte.
             //Não esperado para TipoViagem Frota.
             with Subcontratante do
             begin
               NomeOuRazaoSocial := '';
               CpfOuCnpj := '';

               EMail := '';
               ResponsavelPeloPagamento := False;

               Endereco.Bairro := '';
               Endereco.Rua := '';
               Endereco.Numero := '';
               Endereco.Complemento := '';
               Endereco.CEP := '';
               Endereco.CodigoMunicipio := 0;

               Telefones.Celular.DDD := 0;
               Telefones.Celular.Numero := 0;

               Telefones.Fixo.DDD := 0;
               Telefones.Fixo.Numero := 0;

               Telefones.Fax.DDD := 0;
               Telefones.Fax.Numero := 0;
             end;

             // Aquele que receberá as mercadorias transportadas em consignação,
             //indicado no cadastramento da Operação de Transporte ou nos respectivos documentos fiscais.
             //Não esperado para TipoViagem Frota
             with Consignatario do
             begin
               NomeOuRazaoSocial := '';
               CpfOuCnpj := '';

               EMail := '';
               ResponsavelPeloPagamento := False;

               Endereco.Bairro := '';
               Endereco.Rua := '';
               Endereco.Numero := '';
               Endereco.Complemento := '';
               Endereco.CEP := '';
               Endereco.CodigoMunicipio := 0;

               Telefones.Celular.DDD := 0;
               Telefones.Celular.Numero := 0;

               Telefones.Fixo.DDD := 0;
               Telefones.Fixo.Numero := 0;

               Telefones.Fax.DDD := 0;
               Telefones.Fax.Numero := 0;
             end;

             //Pessoa (física ou jurídica) que contratou o frete pela transportadora.
             //Na emissão com TipoViagem Padrão seu preenchimento é obrigatório.
             //Na emissão com TipoViagem TAC_Agregado o campo não deve ser preenchido.
             with TomadorServico do
             begin
               NomeOuRazaoSocial := '';
               CpfOuCnpj := '';

               EMail := '';
               ResponsavelPeloPagamento := False;

               Endereco.Bairro := '';
               Endereco.Rua := '';
               Endereco.Numero := '';
               Endereco.Complemento := '';
               Endereco.CEP := '';
               Endereco.CodigoMunicipio := 0;

               Telefones.Celular.DDD := 0;
               Telefones.Celular.Numero := 0;

               Telefones.Fixo.DDD := 0;
               Telefones.Fixo.Numero := 0;

               Telefones.Fax.DDD := 0;
               Telefones.Fax.Numero := 0;
             end;

             with Veiculos.New do
             begin
               Placa := 'AAA1234';
             end;

             //Informar um CIOT (se existente) que esteja relacionado à operação de transporte.
             //Por exemplo: No caso da presença de um Subcontratante na operação de transporte
             //informar o CIOT onde o Subcontratante foi o Contratado
             CodigoIdentificacaoOperacaoPrincipal := '';

             with ObservacoesAoTransportador.New do
             begin
               Mensagem := 'teste de obsevação ao transportador';
             end;

             with ObservacoesAoCredenciado.New do
             begin
               Mensagem := 'teste de obsevação ao Credenciado';
             end;

             EntregaDocumentacao := edRedeCredenciada; //Ver como funciona
             QuantidadeSaques := 0; //Quantidade saques que serão realizados pelo Contratado na operação de transporte.
             QuantidadeTransferencias := 0; //Quantidade de Transferências  Bancárias que serão solicitadas pelo Contratado na operação de transporte.
             ValorSaques := 0;
             ValorTransferencias := 0;

             // se o tipo de viagem for padrão (TipoViagem := Padrao) devemos
             // informar o valor tpNaoAplicavel ao campo CodigoTipoCarga
             // valores permitidos para o campo:
             // tpNaoAplicavel, tpGranelsolido, tpGranelLiquido, tpFrigorificada,
             // tpConteinerizada, tpCargaGeral, tpNeogranel, tpPerigosaGranelSolido,
             // tpPerigosaGranelLiquido, tpPerigosaCargaFrigorificada,
             // tpPerigosaConteinerizada, tpPerigosaCargaGeral
             CodigoTipoCarga := tpNaoAplicavel;
             AltoDesempenho := True;
             DestinacaoComercial := True;
             FreteRetorno := False;
             CepRetorno := '';
             DistanciaRetorno := 100;
           end;
         end;

      5: begin
           //Adicionar uma Viagem a uma Operação de Transporte existente,
           //desde que a mesma não tenha ultrapassado o prazo do fim da viagem,
           //esteja cancelada ou encerrada.
           Integradora.Operacao := opAdicionarViagem;

           with AdicionarViagem do
           begin
             CodigoIdentificacaoOperacao := '123';

             //Somente para TipoViagem TAC_Agregado
             with Viagens.New do
             begin
               DocumentoViagem := 'CTe';
               CodigoMunicipioOrigem := 4212908; //Pinhalzinho SC
               CodigoMunicipioDestino := 4217303; //Saudades SC
               CepOrigem := '';
               CepDestino := '';
               DistanciaPercorrida := 100;

               Valores.TotalOperacao := 50;
               Valores.TotalViagem := 50;
               Valores.TotalDeAdiantamento := 10;
               Valores.TotalDeQuitacao := 10;
               Valores.Combustivel := 20;
               Valores.Pedagio := 10;
               Valores.OutrosCreditos := 1;
               Valores.JustificativaOutrosCreditos := 'Teste';
               Valores.Seguro := 10;
               Valores.OutrosDebitos := 1;
               Valores.JustificativaOutrosDebitos := 'Teste outros Debitos';

               TipoPagamento := TransferenciaBancaria;

               with NotasFiscais.New do
               begin
                 Numero := '12345';
                 Serie := '1';
                 Data := Date;
                 ValorTotal := 100;

                 ValorDaMercadoriaPorUnidade := 100;
                 CodigoNCMNaturezaCarga := 5501;
                 DescricaoDaMercadoria := 'Produto Teste';
                 UnidadeDeMedidaDaMercadoria := umKg;
                 TipoDeCalculo := SemQuebra;
                 ValorDoFretePorUnidadeDeMercadoria := 0; //Se tiver quebra deve ser informado
                 QuantidadeDaMercadoriaNoEmbarque := 1;

                 ToleranciaDePerdaDeMercadoria.Tipo := tpPorcentagem;
                 ToleranciaDePerdaDeMercadoria.Valor := 2; //Valor da tolerância admitido.

                 DiferencaDeFrete.Tipo := Integral;
                 DiferencaDeFrete.Base := QuantidadeDesembarque;

                 DiferencaDeFrete.Tolerancia.Tipo := tpPorcentagem;
                 DiferencaDeFrete.Tolerancia.Valor := 5; //Valor da tolerância admitido(Nenhum: 0; Porcentagem: 0.00 – 100.00; Absoluto: Livre)

                 DiferencaDeFrete.MargemGanho.Tipo := tpPorcentagem;
                 DiferencaDeFrete.MargemGanho.Valor := 5;

                 DiferencaDeFrete.MargemPerda.Tipo := tpPorcentagem;
                 DiferencaDeFrete.MargemPerda.Valor := 5;
               end;
             end;

             with Pagamentos.New do
             begin
               IdPagamentoCliente := '1';
               DataDeLiberacao := Date;
               Valor := 10;
               TipoPagamento := TransferenciaBancaria; //TransferenciaBancaria(EmissaoGratuita = true); eFRETE (EmissaoGratuita = false)
               Categoria := tcpSemCategoria;//Para os TipoViagem Frota e TAC_Agregado são suportadas as Categorias Frota e SemCategoria. Para o TipoViagem Padrão todas as categorias são suportadas.
               Documento := ''; //Documento relacionado a viagem.

               InformacoesBancarias.InstituicaoBancaria := '756'; //Bancoob
               InformacoesBancarias.Agencia := '';
               InformacoesBancarias.Conta := '';
               InformacoesBancarias.TipoConta := tcContaCorrente;

               InformacaoAdicional := '';
               //CNPJ que deve ser gerada a Nota Fiscal do abastecimento,
               //sendo da mesma raíz do CNPJ da matriz do contratante,
               //apenas aplicável para Categoria Frota (Abastecimento)
               CnpjFilialAbastecimento := AdicionarOperacao.MatrizCNPJ;
             end;

             NaoAdicionarParcialmente := True;
           end;
         end;

      6: begin
           //Adicionar um registro para Pagamentos em uma Operação de Transporte.
           Integradora.Operacao := opAdicionarPagamento;

           with AdicionarPagamento do
           begin
             CodigoIdentificacaoOperacao := '123';

             with Pagamentos.New do
             begin
               IdPagamentoCliente := '1';
               DataDeLiberacao := Date;
               Valor := 10;
               TipoPagamento := TransferenciaBancaria; //TransferenciaBancaria(EmissaoGratuita = true); eFRETE (EmissaoGratuita = false)
               Categoria := tcpSemCategoria;//Para os TipoViagem Frota e TAC_Agregado são suportadas as Categorias Frota e SemCategoria. Para o TipoViagem Padrão todas as categorias são suportadas.
               Documento := ''; //Documento relacionado a viagem.

               InformacoesBancarias.InstituicaoBancaria := '756'; //Bancoob
               InformacoesBancarias.Agencia := '';
               InformacoesBancarias.Conta := '';
               InformacoesBancarias.TipoConta := tcContaCorrente;

               InformacaoAdicional := '';
               //CNPJ que deve ser gerada a Nota Fiscal do abastecimento,
               //sendo da mesma raíz do CNPJ da matriz do contratante,
               //apenas aplicável para Categoria Frota (Abastecimento)
               CnpjFilialAbastecimento := AdicionarOperacao.MatrizCNPJ;
             end;
           end;
         end;

      7: begin
           // Obter Código Identificação Operação Transp.
           Integradora.Operacao := opObterCodigoIOT;

           ObterCodigoOperacaoTransporte.MatrizCNPJ := edtEmitCNPJ.Text;
           ObterCodigoOperacaoTransporte.IdOperacaoCliente := '501';
         end;

      8: begin
           // Obter Pdf Operação Transporte
           Integradora.Operacao := opObterPdf;

           ObterOperacaoTransportePDF.CodigoIdentificacaoOperacao := '161000017511/1739';
//           ObterOperacaoTransportePDF.DocumentoViagem := '456';
         end;

      9: begin
           // Retificar uma operação de transporte.
           Integradora.Operacao := opRetificar;

           with RetificarOperacao do
           begin
             CodigoIdentificacaoOperacao := '123';
             DataInicioViagem := Now;
             DataFimViagem := Now;
             CodigoNCMNaturezaCarga := 0;
             PesoCarga := 10;
             CodigoMunicipioOrigem := 4212908; //Pinhalzinho SC
             CodigoMunicipioDestino := 4217303; //Saudades SC

             with Veiculos.New do
             begin
               Placa := 'AAA1234';
             end;

             QuantidadeSaques := 0;
             QuantidadeTransferencias := 0;
             ValorSaques := 0;
             ValorTransferencias := 0;
             CodigoTipoCarga := tpCargaGeral;
             CepOrigem := '4800000';
             CepDestino := '4800000';
             DistanciaPercorrida := 100;
           end;
         end;

     10: begin
           //Cancelar uma operação de transporte
           Integradora.Operacao := opCancelar;

           CancelarOperacao.CodigoIdentificacaoOperacao := '123';
           CancelarOperacao.Motivo := 'Erro na digitacao';
         end;

     11: begin
           //Cancelar um pagamento programado para uma operação de transporte.
           Integradora.Operacao := opCancelarPagamento;

           CancelarPagamento.CodigoIdentificacaoOperacao := '123';
           CancelarPagamento.IdPagamentoCliente := '456';
           CancelarPagamento.Motivo := 'Erro na digitacao';
         end;

     12: begin
           //Encerrar uma operação de transporte existente que não esteja cancelada.
           Integradora.Operacao := opEncerrar;

           with EncerrarOperacao do
           begin
             CodigoIdentificacaoOperacao := '123';
             PesoCarga := 100;

             //Somente para TipoViagem TAC_Agregado
             with Viagens.New do
             begin
               DocumentoViagem := 'CTe';
               CodigoMunicipioOrigem := 4212908; //Pinhalzinho SC
               CodigoMunicipioDestino := 4217303; //Saudades SC
               CepOrigem := '';
               CepDestino := '';
               DistanciaPercorrida := 100;

               Valores.TotalOperacao := 50;
               Valores.TotalViagem := 50;
               Valores.TotalDeAdiantamento := 10;
               Valores.TotalDeQuitacao := 10;
               Valores.Combustivel := 20;
               Valores.Pedagio := 10;
               Valores.OutrosCreditos := 1;
               Valores.JustificativaOutrosCreditos := 'Teste';
               Valores.Seguro := 10;
               Valores.OutrosDebitos := 1;
               Valores.JustificativaOutrosDebitos := 'Teste outros Debitos';

               TipoPagamento := TransferenciaBancaria;

               with NotasFiscais.New do
               begin
                 Numero := '12345';
                 Serie := '1';
                 Data := Date;
                 ValorTotal := 100;

                 ValorDaMercadoriaPorUnidade := 100;
                 CodigoNCMNaturezaCarga := 5501;
                 DescricaoDaMercadoria := 'Produto Teste';
                 UnidadeDeMedidaDaMercadoria := umKg;
                 TipoDeCalculo := SemQuebra;
                 ValorDoFretePorUnidadeDeMercadoria := 0; //Se tiver quebra deve ser informado
                 QuantidadeDaMercadoriaNoEmbarque := 1;

                 ToleranciaDePerdaDeMercadoria.Tipo := tpPorcentagem;
                 ToleranciaDePerdaDeMercadoria.Valor := 2; //Valor da tolerância admitido.

                 DiferencaDeFrete.Tipo := Integral;
                 DiferencaDeFrete.Base := QuantidadeDesembarque;

                 DiferencaDeFrete.Tolerancia.Tipo := tpPorcentagem;
                 DiferencaDeFrete.Tolerancia.Valor := 5; //Valor da tolerância admitido(Nenhum: 0; Porcentagem: 0.00 – 100.00; Absoluto: Livre)

                 DiferencaDeFrete.MargemGanho.Tipo := tpPorcentagem;
                 DiferencaDeFrete.MargemGanho.Valor := 5;

                 DiferencaDeFrete.MargemPerda.Tipo := tpPorcentagem;
                 DiferencaDeFrete.MargemPerda.Valor := 5;
               end;
             end;

             with Pagamentos.New do
             begin
               IdPagamentoCliente := '1';
               DataDeLiberacao := Date;
               Valor := 10;
               TipoPagamento := TransferenciaBancaria; //TransferenciaBancaria(EmissaoGratuita = true); eFRETE (EmissaoGratuita = false)
               Categoria := tcpSemCategoria;//Para os TipoViagem Frota e TAC_Agregado são suportadas as Categorias Frota e SemCategoria. Para o TipoViagem Padrão todas as categorias são suportadas.
               Documento := ''; //Documento relacionado a viagem.

               InformacoesBancarias.InstituicaoBancaria := '756'; //Bancoob
               InformacoesBancarias.Agencia := '';
               InformacoesBancarias.Conta := '';
               InformacoesBancarias.TipoConta := tcContaCorrente;

               InformacaoAdicional := '';
               //CNPJ que deve ser gerada a Nota Fiscal do abastecimento,
               //sendo da mesma raíz do CNPJ da matriz do contratante,
               //apenas aplicável para Categoria Frota (Abastecimento)
               CnpjFilialAbastecimento := AdicionarOperacao.MatrizCNPJ;
             end;

             //Não esperado para TipoViagem Frota.
             with Impostos do
             begin
               IRRF := 0;
               SestSenat := 0;
               INSS := 0;
               ISSQN := 0;
               OutrosImpostos := 0;
               DescricaoOutrosImpostos := '';
             end;

             QuantidadeSaques := 0;
             QuantidadeTransferencias := 0;
             ValorSaques := 0;
             ValorTransferencias := 0;
           end;
         end;

     13: begin
           // Logout - Encerra acesso ao Sistema
           Integradora.Operacao := opLogout;
         end;

     14: begin
           Integradora.Operacao := opConsultarTipoCarga;
         end;

     15: begin
           Integradora.Operacao := opAlterarDataLiberacaoPagamento;

           with AlterarDataLiberacaoPagamento do
           begin
             CodigoIdentificacaoOperacao := '123';
             // Identificador do pagamento no sistema do Cliente.
             IdPagamentoCliente := '456';
             DataDeLiberacao := StrToDate('10/05/2020');
             Motivo := 'Acordo entre as partes';
           end;
         end;
    end;
  end;
end;

procedure TfrmACBrCIOT.AtualizarSSLLibsCombo;
begin
  cbSSLLib.ItemIndex     := Integer(ACBrCIOT1.Configuracoes.Geral.SSLLib);
  cbCryptLib.ItemIndex   := Integer(ACBrCIOT1.Configuracoes.Geral.SSLCryptLib);
  cbHttpLib.ItemIndex    := Integer(ACBrCIOT1.Configuracoes.Geral.SSLHttpLib);
  cbXmlSignLib.ItemIndex := Integer(ACBrCIOT1.Configuracoes.Geral.SSLXmlSignLib);

  cbSSLType.Enabled := (ACBrCIOT1.Configuracoes.Geral.SSLHttpLib in [httpWinHttp, httpOpenSSL]);
end;

procedure TfrmACBrCIOT.btnCNPJClick(Sender: TObject);
begin
  ShowMessage(ACBrCIOT1.SSL.CertCNPJ);
end;

procedure TfrmACBrCIOT.btnCriarEnviarClick(Sender: TObject);
var
  i: Integer;
begin
  ACBrCIOT1.Contratos.Clear;
  AlimentarComponente;
  ACBrCIOT1.Enviar;

  MemoResp.Lines.Text   := UTF8Encode(ACBrCIOT1.WebServices.CIOTEnviar.RetornoWS);
  memoRespWS.Lines.Text := UTF8Encode(ACBrCIOT1.WebServices.CIOTEnviar.RetWS);
  LoadXML(MemoResp.Lines.Text, WBResposta);

  pgRespostas.ActivePageIndex := 2;

  with ACBrCIOT1.WebServices.CIOTEnviar.RetornoEnvio.RetEnvio do
  begin
    MemoDados.Lines.Clear;
    MemoDados.Lines.Add('Retorno do Envio');
    MemoDados.Lines.Add('Versão...........: '+ IntToStr(Versao));
    MemoDados.Lines.Add('Sucesso..........: '+ Sucesso);
    MemoDados.Lines.Add('Protocolo Serviço: '+ ProtocoloServico);
    MemoDados.Lines.Add('');

    if Mensagem <> '' then
    begin
      MemoDados.Lines.Add('Retorno do Envio (Exceção)');
      MemoDados.Lines.Add('Mensagem: '+ Mensagem);
      MemoDados.Lines.Add('Código..: '+ Codigo);
    end
    else
    begin
      MemoDados.Lines.Add('Token........................: '+ Token);
      MemoDados.Lines.Add('Código Identificação Operação: '+ CodigoIdentificacaoOperacao);
      MemoDados.Lines.Add('Data.........................: '+ DateTimeToStr(Data));
      MemoDados.Lines.Add('Protocolo....................: '+ Protocolo);
      MemoDados.Lines.Add('Data Retificação.............: '+ DateTimeToStr(DataRetificacao));
      MemoDados.Lines.Add('Quantidade Viagens...........: '+ IntToStr(QuantidadeViagens));
      MemoDados.Lines.Add('Quantidade Pagamentos,.......: '+ IntToStr(QuantidadePagamentos));
      MemoDados.Lines.Add('Id Pagamento Cliente.........: '+ IdPagamentoCliente);

      // A propriedade Token só é retornado caso o componente não esteja
      // configurado com o certificado digital e o serviço Login foi executado.
      sToken := Token;

      if DocumentoViagem.Count > 0 then
      begin
        MemoDados.Lines.Add('Documento Viagem');

        for i := 0 to DocumentoViagem.Count -1 do
           MemoDados.Lines.Add('Mensagem: '+ DocumentoViagem[i].Mensagem);
      end;

      if DocumentoPagamento.Count > 0 then
      begin
        MemoDados.Lines.Add('Documento Pagamento');

        for i := 0 to DocumentoPagamento.Count -1 do
           MemoDados.Lines.Add('Mensagem: '+ DocumentoPagamento[i].Mensagem);
      end;

      if TipoCarga.Count > 0 then
      begin
        MemoDados.Lines.Add('  ');
        MemoDados.Lines.Add('  ');
        MemoDados.Lines.Add('********* Tipos Cargas ***********');
        MemoDados.Lines.Add('  ');

        for i := 0 to TipoCarga.Count -1 do
        begin
           MemoDados.Lines.Add(IntToStr(TipoCarga[i].Codigo) +
                               ' - '+ TipoCargaToStr(TipoCarga[i].Descricao));
        end;
      end;
    end;
  end;

  ACBrCIOT1.Contratos.Clear;
end;

procedure TfrmACBrCIOT.btnDataValidadeClick(Sender: TObject);
begin
  ShowMessage(FormatDateBr(ACBrCIOT1.SSL.CertDataVenc));
end;

procedure TfrmACBrCIOT.btnEnviarCiotEmailClick(Sender: TObject);
var
  Para: String;
  CC: Tstrings;
begin
  Para := '';
  if not(InputQuery('Enviar Email', 'Email de destino', Para)) then
    exit;

  OpenDialog1.Title := 'Selecione o CIOT';
  OpenDialog1.DefaultExt := '*-CIOT.xml';
  OpenDialog1.Filter := 'Arquivos RegBol (*-CIOT.xml)|*-CIOT.xml|Arquivos XML (*.xml)|*.xml|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrCIOT1.Configuracoes.Arquivos.PathSalvar;

  if not OpenDialog1.Execute then
    Exit;

  ACBrCIOT1.Contratos.Clear;
  ACBrCIOT1.Contratos.LoadFromFile(OpenDialog1.FileName);
  CC := TStringList.Create;
  try
    //CC.Add('email_1@provedor.com'); //especifique um email válido
    //CC.Add('email_2@provedor.com.br'); //especifique um email válido
    ConfigurarEmail;
    ACBrCIOT1.Contratos.Items[0].EnviarEmail(Para
      , edtEmailAssunto.Text
      , mmEmailMsg.Lines
      , False // Enviar PDF junto
      , CC   // Lista com emails que serão enviado cópias - TStrings
      , nil   // Lista de RegBolxos - TStrings
      );
  finally
    CC.Free;
  end;
end;

procedure TfrmACBrCIOT.btnGerarCIOTClick(Sender: TObject);
var
  vAux : String;
  Codigo: Integer;

begin
  vAux := '';
  if not (InputQuery('Consultar por Descrição', 'Nome da Cidade', vAux)) then
    exit;

  Codigo := ObterCodigoMunicipio(vAux, 'SP', 'C:\Erp\Txt\Blt' );

  ShowMessage('Codigo: ' + IntToStr(Codigo));
end;

procedure TfrmACBrCIOT.btnIssuerNameClick(Sender: TObject);
begin
  ShowMessage(ACBrCIOT1.SSL.CertIssuerName + sLineBreak + sLineBreak +
              'Certificadora: ' + ACBrCIOT1.SSL.CertCertificadora);
end;

procedure TfrmACBrCIOT.btnLeituraX509Click(Sender: TObject);
//var
//  Erro, AName: String;
begin
  with ACBrCIOT1.SSL do
  begin
     CarregarCertificadoPublico(MemoDados.Lines.Text);
     MemoResp.Lines.Add(CertIssuerName);
     MemoResp.Lines.Add(CertRazaoSocial);
     MemoResp.Lines.Add(CertCNPJ);
     MemoResp.Lines.Add(CertSubjectName);
     MemoResp.Lines.Add(CertNumeroSerie);

    //MemoDados.Lines.LoadFromFile('c:\temp\teste2.xml');
    //MemoResp.Lines.Text := Assinar(MemoDados.Lines.Text, 'Entrada', 'Parametros');
    //Erro := '';
    //if VerificarAssinatura(MemoResp.Lines.Text, Erro, 'Parametros' ) then
    //  ShowMessage('OK')
    //else
    //  ShowMessage('ERRO: '+Erro)

    pgRespostas.ActivePageIndex := 0;
  end;
end;

procedure TfrmACBrCIOT.btnNumSerieClick(Sender: TObject);
begin
  ShowMessage(ACBrCIOT1.SSL.CertNumeroSerie);
end;

procedure TfrmACBrCIOT.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
end;

procedure TfrmACBrCIOT.btnSha256Click(Sender: TObject);
var
  Ahash: AnsiString;
begin
  Ahash := ACBrCIOT1.SSL.CalcHash(Edit1.Text, dgstSHA256, outBase64, cbAssinar.Checked);
  MemoResp.Lines.Add( Ahash );
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrCIOT.btnSubNameClick(Sender: TObject);
begin
  ShowMessage(ACBrCIOT1.SSL.CertSubjectName + sLineBreak + sLineBreak +
              'Razão Social: ' + ACBrCIOT1.SSL.CertRazaoSocial);
end;

procedure TfrmACBrCIOT.Button2Click(Sender: TObject);
var
  vAux : String;
  Nome: string;
begin
  vAux := '';
  if not (InputQuery('Consultar por Codigo', 'Codigo da Cidade', vAux)) then
    exit;

  Nome := ObterNomeMunicipio(UFparaCodigoUF('SP'), vAux, 'C:\Erp\Txt\Blt' );

  ShowMessage('Nome: ' + Nome);
end;

procedure TfrmACBrCIOT.cbCryptLibChange(Sender: TObject);
begin
  try
    if cbCryptLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLCryptLib := TSSLCryptLib(cbCryptLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCIOT.cbHttpLibChange(Sender: TObject);
begin
  try
    if cbHttpLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLHttpLib := TSSLHttpLib(cbHttpLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCIOT.cbSSLLibChange(Sender: TObject);
begin
  try
    if cbSSLLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLLib := TSSLLib(cbSSLLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCIOT.cbSSLTypeChange(Sender: TObject);
begin
  if cbSSLType.ItemIndex <> -1 then
    ACBrCIOT1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);
end;

procedure TfrmACBrCIOT.cbXmlSignLibChange(Sender: TObject);
begin
  try
    if cbXmlSignLib.ItemIndex <> -1 then
      ACBrCIOT1.Configuracoes.Geral.SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);
  finally
    AtualizarSSLLibsCombo;
  end;
end;

procedure TfrmACBrCIOT.FormCreate(Sender: TObject);
var
  T: TSSLLib;
  I: TpcnTipoEmissao;
  K: TVersaoCIOT;
  U: TSSLCryptLib;
  V: TSSLHttpLib;
  X: TSSLXmlSignLib;
  Y: TSSLType;
  Integradora: TCIOTIntegradora;
begin
  // Variavel para armazenar o Token retornado pelo serviço Login
  // Necessário quando o componente não esta configurado com o
  // Certificado Digital
  sToken := '';

  cbSSLLib.Items.Clear;
  for T := Low(TSSLLib) to High(TSSLLib) do
    cbSSLLib.Items.Add( GetEnumName(TypeInfo(TSSLLib), integer(T) ) );
  cbSSLLib.ItemIndex := 0;

  cbCryptLib.Items.Clear;
  for U := Low(TSSLCryptLib) to High(TSSLCryptLib) do
    cbCryptLib.Items.Add( GetEnumName(TypeInfo(TSSLCryptLib), integer(U) ) );
  cbCryptLib.ItemIndex := 0;

  cbHttpLib.Items.Clear;
  for V := Low(TSSLHttpLib) to High(TSSLHttpLib) do
    cbHttpLib.Items.Add( GetEnumName(TypeInfo(TSSLHttpLib), integer(V) ) );
  cbHttpLib.ItemIndex := 0;

  cbXmlSignLib.Items.Clear;
  for X := Low(TSSLXmlSignLib) to High(TSSLXmlSignLib) do
    cbXmlSignLib.Items.Add( GetEnumName(TypeInfo(TSSLXmlSignLib), integer(X) ) );
  cbXmlSignLib.ItemIndex := 0;

  cbSSLType.Items.Clear;
  for Y := Low(TSSLType) to High(TSSLType) do
    cbSSLType.Items.Add( GetEnumName(TypeInfo(TSSLType), integer(Y) ) );
  cbSSLType.ItemIndex := 0;

  cbFormaEmissao.Items.Clear;
  for I := Low(TpcnTipoEmissao) to High(TpcnTipoEmissao) do
     cbFormaEmissao.Items.Add( GetEnumName(TypeInfo(TpcnTipoEmissao), integer(I) ) );
  cbFormaEmissao.ItemIndex := 0;

  cbVersaoDF.Items.Clear;
  for K := Low(TVersaoCIOT) to High(TVersaoCIOT) do
     cbVersaoDF.Items.Add( GetEnumName(TypeInfo(TVersaoCIOT), integer(K) ) );
  cbVersaoDF.ItemIndex := 0;

  cbbIntegradora.Items.Clear;
  for Integradora := Low(TCIOTIntegradora) to High(TCIOTIntegradora) do
     cbbIntegradora.Items.Add( GetEnumName(TypeInfo(TCIOTIntegradora), integer(Integradora) ) );
  cbbIntegradora.ItemIndex := 0;

  LerConfiguracao;
  pgRespostas.ActivePageIndex := 0;
end;

procedure TfrmACBrCIOT.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    Ini.WriteInteger('Certificado', 'SSLLib',     cbSSLLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'CryptLib',   cbCryptLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'HttpLib',    cbHttpLib.ItemIndex);
    Ini.WriteInteger('Certificado', 'XmlSignLib', cbXmlSignLib.ItemIndex);
    Ini.WriteString( 'Certificado', 'Caminho',    edtCaminho.Text);
    Ini.WriteString( 'Certificado', 'Senha',      edtSenha.Text);
    Ini.WriteString( 'Certificado', 'NumSerie',   edtNumSerie.Text);

    Ini.WriteBool(   'Geral', 'AtualizarXML',     cbxAtualizarXML.Checked);
    Ini.WriteBool(   'Geral', 'ExibirErroSchema', cbxExibirErroSchema.Checked);
    Ini.WriteString( 'Geral', 'FormatoAlerta',    edtFormatoAlerta.Text);
    Ini.WriteInteger('Geral', 'FormaEmissao',     cbFormaEmissao.ItemIndex);
    Ini.WriteInteger('Geral', 'VersaoDF',         cbVersaoDF.ItemIndex);
    Ini.WriteBool(   'Geral', 'RetirarAcentos',   cbxRetirarAcentos.Checked);
    Ini.WriteBool(   'Geral', 'Salvar',           ckSalvar.Checked);
    Ini.WriteString( 'Geral', 'PathSalvar',       edtPathLogs.Text);
    Ini.WriteString( 'Geral', 'PathSchemas',      edtPathSchemas.Text);
    Ini.WriteInteger( 'Geral','Integradora',      cbbIntegradora.ItemIndex);
    Ini.WriteString( 'Geral', 'UsuarioWebS',      edtUsuarioWebService.Text);
    Ini.WriteString( 'Geral', 'SenhaWebS',        edtSenhaWebService.Text);
    Ini.WriteString( 'Geral', 'HashIntegrador',   edtHashIntegrador.Text);

    Ini.WriteString( 'WebService', 'UF',         cbUF.Text);
    Ini.WriteInteger('WebService', 'Ambiente',   rgTipoAmb.ItemIndex);
    Ini.WriteBool(   'WebService', 'Visualizar', cbxVisualizar.Checked);
    Ini.WriteBool(   'WebService', 'SalvarSOAP', cbxSalvarSOAP.Checked);
    Ini.WriteBool(   'WebService', 'AjustarAut', cbxAjustarAut.Checked);
    Ini.WriteString( 'WebService', 'Aguardar',   edtAguardar.Text);
    Ini.WriteString( 'WebService', 'Tentativas', edtTentativas.Text);
    Ini.WriteString( 'WebService', 'Intervalo',  edtIntervalo.Text);
    Ini.WriteInteger('WebService', 'TimeOut',    seTimeOut.Value);
    Ini.WriteInteger('WebService', 'SSLType',    cbSSLType.ItemIndex);

    Ini.WriteString('Proxy', 'Host',  edtProxyHost.Text);
    Ini.WriteString('Proxy', 'Porta', edtProxyPorta.Text);
    Ini.WriteString('Proxy', 'User',  edtProxyUser.Text);
    Ini.WriteString('Proxy', 'Pass',  edtProxySenha.Text);

    Ini.WriteBool(  'Arquivos', 'Salvar',           cbxSalvarArqs.Checked);
    Ini.WriteBool(  'Arquivos', 'PastaMensal',      cbxPastaMensal.Checked);
    Ini.WriteBool(  'Arquivos', 'AddLiteral',       cbxAdicionaLiteral.Checked);
    Ini.WriteBool(  'Arquivos', 'EmissaoPathCIOT',  cbxEmissaoPathCIOT.Checked);
    Ini.WriteBool(  'Arquivos', 'SalvarPathEvento', cbxSalvaPathEvento.Checked);

    if (edtCaminho.Text = '') and (edtSenha.Text = '') and (edtNumSerie.Text = '') then
      cbxSepararPorCNPJ.Checked := False;

    Ini.WriteBool(  'Arquivos', 'SepararPorCNPJ',   cbxSepararPorCNPJ.Checked);
    Ini.WriteBool(  'Arquivos', 'SepararPorModelo', cbxSepararPorModelo.Checked);
    Ini.WriteString('Arquivos', 'PathCIOT',         edtPathCIOT.Text);
    Ini.WriteString('Arquivos', 'PathEvento',       edtPathEvento.Text);

    Ini.WriteString('Emitente', 'CNPJ',        edtEmitCNPJ.Text);
    Ini.WriteString('Emitente', 'IE',          edtEmitIE.Text);
    Ini.WriteString('Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString('Emitente', 'Fantasia',    edtEmitFantasia.Text);
    Ini.WriteString('Emitente', 'Fone',        edtEmitFone.Text);
    Ini.WriteString('Emitente', 'CEP',         edtEmitCEP.Text);
    Ini.WriteString('Emitente', 'Logradouro',  edtEmitLogradouro.Text);
    Ini.WriteString('Emitente', 'Numero',      edtEmitNumero.Text);
    Ini.WriteString('Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString('Emitente', 'Bairro',      edtEmitBairro.Text);
    Ini.WriteString('Emitente', 'CodCidade',   edtEmitCodCidade.Text);
    Ini.WriteString('Emitente', 'Cidade',      edtEmitCidade.Text);
    Ini.WriteString('Emitente', 'UF',          edtEmitUF.Text);

    Ini.WriteString('Email', 'Host',    edtSmtpHost.Text);
    Ini.WriteString('Email', 'Port',    edtSmtpPort.Text);
    Ini.WriteString('Email', 'User',    edtSmtpUser.Text);
    Ini.WriteString('Email', 'Pass',    edtSmtpPass.Text);
    Ini.WriteString('Email', 'Assunto', edtEmailAssunto.Text);
    Ini.WriteBool(  'Email', 'SSL',     cbEmailSSL.Checked );

    StreamMemo := TMemoryStream.Create;
    mmEmailMsg.Lines.SaveToStream(StreamMemo);
    StreamMemo.Seek(0,soFromBeginning);

    Ini.WriteBinaryStream('Email', 'Mensagem', StreamMemo);

    StreamMemo.Free;

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrCIOT.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrCIOT.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrCIOT.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrCIOT.lblMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold,fsUnderline];
end;

procedure TfrmACBrCIOT.lblMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsBold];
end;

procedure TfrmACBrCIOT.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrCIOT.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
  StreamMemo: TMemoryStream;
begin
  IniFile := ChangeFileExt(Application.ExeName, '.ini');

  Ini := TIniFile.Create(IniFile);
  try
    cbSSLLib.ItemIndex     := Ini.ReadInteger('Certificado', 'SSLLib',     0);
    cbCryptLib.ItemIndex   := Ini.ReadInteger('Certificado', 'CryptLib',   0);
    cbHttpLib.ItemIndex    := Ini.ReadInteger('Certificado', 'HttpLib',    0);
    cbXmlSignLib.ItemIndex := Ini.ReadInteger('Certificado', 'XmlSignLib', 0);
    edtCaminho.Text        := Ini.ReadString( 'Certificado', 'Caminho',    '');
    edtSenha.Text          := Ini.ReadString( 'Certificado', 'Senha',      '');
    edtNumSerie.Text       := Ini.ReadString( 'Certificado', 'NumSerie',   '');

    cbxAtualizarXML.Checked     := Ini.ReadBool(   'Geral', 'AtualizarXML',     True);
    cbxExibirErroSchema.Checked := Ini.ReadBool(   'Geral', 'ExibirErroSchema', True);
    edtFormatoAlerta.Text       := Ini.ReadString( 'Geral', 'FormatoAlerta',    'TAG:%TAGNIVEL% ID:%ID%/%TAG%(%DESCRICAO%) - %MSG%.');
    cbFormaEmissao.ItemIndex    := Ini.ReadInteger('Geral', 'FormaEmissao',     0);

    cbVersaoDF.ItemIndex      := Ini.ReadInteger('Geral', 'VersaoDF',       0);
    ckSalvar.Checked          := Ini.ReadBool(   'Geral', 'Salvar',         True);
    cbxRetirarAcentos.Checked := Ini.ReadBool(   'Geral', 'RetirarAcentos', True);
    edtPathLogs.Text          := Ini.ReadString( 'Geral', 'PathSalvar',     PathWithDelim(ExtractFilePath(Application.ExeName))+'Logs');
    edtPathSchemas.Text       := Ini.ReadString( 'Geral', 'PathSchemas',    PathWithDelim(ExtractFilePath(Application.ExeName))+'Schemas\'+GetEnumName(TypeInfo(TVersaoCIOT), integer(cbVersaoDF.ItemIndex) ));
    cbbIntegradora.ItemIndex  := Ini.ReadInteger('Geral', 'Integradora',    1);
    edtUsuarioWebService.Text := Ini.ReadString( 'Geral', 'UsuarioWebS',    '');
    edtSenhaWebService.Text   := Ini.ReadString( 'Geral', 'SenhaWebS',      '');
    edtHashIntegrador.Text    := Ini.ReadString( 'Geral', 'HashIntegrador', '');

    cbUF.ItemIndex := cbUF.Items.IndexOf(Ini.ReadString('WebService', 'UF', 'SP'));

    rgTipoAmb.ItemIndex   := Ini.ReadInteger('WebService', 'Ambiente',   0);
    cbxVisualizar.Checked := Ini.ReadBool(   'WebService', 'Visualizar', False);
    cbxSalvarSOAP.Checked := Ini.ReadBool(   'WebService', 'SalvarSOAP', False);
    cbxAjustarAut.Checked := Ini.ReadBool(   'WebService', 'AjustarAut', False);
    edtAguardar.Text      := Ini.ReadString( 'WebService', 'Aguardar',   '0');
    edtTentativas.Text    := Ini.ReadString( 'WebService', 'Tentativas', '5');
    edtIntervalo.Text     := Ini.ReadString( 'WebService', 'Intervalo',  '0');
    seTimeOut.Value       := Ini.ReadInteger('WebService', 'TimeOut',    5000);
    cbSSLType.ItemIndex   := Ini.ReadInteger('WebService', 'SSLType',    0);

    edtProxyHost.Text  := Ini.ReadString('Proxy', 'Host',  '');
    edtProxyPorta.Text := Ini.ReadString('Proxy', 'Porta', '');
    edtProxyUser.Text  := Ini.ReadString('Proxy', 'User',  '');
    edtProxySenha.Text := Ini.ReadString('Proxy', 'Pass',  '');

    cbxSalvarArqs.Checked       := Ini.ReadBool(  'Arquivos', 'Salvar',           false);
    cbxPastaMensal.Checked      := Ini.ReadBool(  'Arquivos', 'PastaMensal',      false);
    cbxAdicionaLiteral.Checked  := Ini.ReadBool(  'Arquivos', 'AddLiteral',       false);
    cbxEmissaoPathCIOT.Checked  := Ini.ReadBool(  'Arquivos', 'EmissaoPathCIOT',  false);
    cbxSalvaPathEvento.Checked  := Ini.ReadBool(  'Arquivos', 'SalvarPathEvento', false);
    cbxSepararPorCNPJ.Checked   := Ini.ReadBool(  'Arquivos', 'SepararPorCNPJ',   false);
    cbxSepararPorModelo.Checked := Ini.ReadBool(  'Arquivos', 'SepararPorModelo', false);
    edtPathCIOT.Text            := Ini.ReadString('Arquivos', 'PathCIOT',         '');
    edtPathEvento.Text          := Ini.ReadString('Arquivos', 'PathEvento',       '');

    edtEmitCNPJ.Text       := Ini.ReadString('Emitente', 'CNPJ',        '');
    edtEmitIE.Text         := Ini.ReadString('Emitente', 'IE',          '');
    edtEmitRazao.Text      := Ini.ReadString('Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text   := Ini.ReadString('Emitente', 'Fantasia',    '');
    edtEmitFone.Text       := Ini.ReadString('Emitente', 'Fone',        '');
    edtEmitCEP.Text        := Ini.ReadString('Emitente', 'CEP',         '');
    edtEmitLogradouro.Text := Ini.ReadString('Emitente', 'Logradouro',  '');
    edtEmitNumero.Text     := Ini.ReadString('Emitente', 'Numero',      '');
    edtEmitComp.Text       := Ini.ReadString('Emitente', 'Complemento', '');
    edtEmitBairro.Text     := Ini.ReadString('Emitente', 'Bairro',      '');
    edtEmitCodCidade.Text  := Ini.ReadString('Emitente', 'CodCidade',   '');
    edtEmitCidade.Text     := Ini.ReadString('Emitente', 'Cidade',      '');
    edtEmitUF.Text         := Ini.ReadString('Emitente', 'UF',          '');

    edtSmtpHost.Text     := Ini.ReadString('Email', 'Host',    '');
    edtSmtpPort.Text     := Ini.ReadString('Email', 'Port',    '');
    edtSmtpUser.Text     := Ini.ReadString('Email', 'User',    '');
    edtSmtpPass.Text     := Ini.ReadString('Email', 'Pass',    '');
    edtEmailAssunto.Text := Ini.ReadString('Email', 'Assunto', '');
    cbEmailSSL.Checked   := Ini.ReadBool(  'Email', 'SSL',     False);

    StreamMemo := TMemoryStream.Create;
    Ini.ReadBinaryStream('Email', 'Mensagem', StreamMemo);
    mmEmailMsg.Lines.LoadFromStream(StreamMemo);
    StreamMemo.Free;

    ConfigurarComponente;
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrCIOT.ConfigurarComponente;
var
  Ok: Boolean;
  PathMensal: string;
begin
  ACBrCIOT1.Configuracoes.Certificados.ArquivoPFX  := edtCaminho.Text;
  ACBrCIOT1.Configuracoes.Certificados.Senha       := edtSenha.Text;
  ACBrCIOT1.Configuracoes.Certificados.NumeroSerie := edtNumSerie.Text;

  ACBrCIOT1.SSL.DescarregarCertificado;

  // Não for informado nenhuma informação a respeito do certificado
  // o componente será configurado para não carregar o certificado.
  ACBrCIOT1.SSL.UseCertificateHTTP := (edtCaminho.Text <> '') or
                              (edtSenha.Text <> '') or (edtNumSerie.Text <> '');

  with ACBrCIOT1.Configuracoes.Geral do
  begin
    SSLLib        := TSSLLib(cbSSLLib.ItemIndex);
    SSLCryptLib   := TSSLCryptLib(cbCryptLib.ItemIndex);
    SSLHttpLib    := TSSLHttpLib(cbHttpLib.ItemIndex);
    SSLXmlSignLib := TSSLXmlSignLib(cbXmlSignLib.ItemIndex);

    AtualizarSSLLibsCombo;

    Salvar           := ckSalvar.Checked;
    ExibirErroSchema := cbxExibirErroSchema.Checked;
    RetirarAcentos   := cbxRetirarAcentos.Checked;
    FormatoAlerta    := edtFormatoAlerta.Text;
    FormaEmissao     := TpcnTipoEmissao(cbFormaEmissao.ItemIndex);
    VersaoDF         := TVersaoCIOT(cbVersaoDF.ItemIndex);
    Integradora      := StrToIntegradora(cbbIntegradora.text);
    Usuario          := edtUsuarioWebService.Text;
    Senha            := edtSenhaWebService.Text;
    HashIntegrador   := edtHashIntegrador.Text;
  end;

  with ACBrCIOT1.Configuracoes.WebServices do
  begin
    UF         := cbUF.Text;
    Ambiente   := StrToTpAmb(Ok,IntToStr(rgTipoAmb.ItemIndex+1));
    Visualizar := cbxVisualizar.Checked;
    Salvar     := cbxSalvarSOAP.Checked;

    AjustaAguardaConsultaRet := cbxAjustarAut.Checked;

    if NaoEstaVazio(edtAguardar.Text)then
      AguardarConsultaRet := ifThen(StrToInt(edtAguardar.Text) < 1000, StrToInt(edtAguardar.Text) * 1000, StrToInt(edtAguardar.Text))
    else
      edtAguardar.Text := IntToStr(AguardarConsultaRet);

    if NaoEstaVazio(edtTentativas.Text) then
      Tentativas := StrToInt(edtTentativas.Text)
    else
      edtTentativas.Text := IntToStr(Tentativas);

    if NaoEstaVazio(edtIntervalo.Text) then
      IntervaloTentativas := ifThen(StrToInt(edtIntervalo.Text) < 1000, StrToInt(edtIntervalo.Text) * 1000, StrToInt(edtIntervalo.Text))
    else
      edtIntervalo.Text := IntToStr(ACBrCIOT1.Configuracoes.WebServices.IntervaloTentativas);

    TimeOut   := seTimeOut.Value;
    ProxyHost := edtProxyHost.Text;
    ProxyPort := edtProxyPorta.Text;
    ProxyUser := edtProxyUser.Text;
    ProxyPass := edtProxySenha.Text;
  end;

  ACBrCIOT1.SSL.SSLType := TSSLType(cbSSLType.ItemIndex);

  with ACBrCIOT1.Configuracoes.Arquivos do
  begin
    Salvar           := cbxSalvarArqs.Checked;
    SepararPorMes    := cbxPastaMensal.Checked;
    AdicionarLiteral := cbxAdicionaLiteral.Checked;
    EmissaoPathCIOT  := cbxEmissaoPathCIOT.Checked;
    SepararPorCNPJ   := cbxSepararPorCNPJ.Checked;
    SepararPorModelo := cbxSepararPorModelo.Checked;
    PathSchemas      := edtPathSchemas.Text;
    PathCIOT         := edtPathCIOT.Text;
    PathMensal       := GetPathCIOT(0);
    PathSalvar       := PathMensal;
  end;
end;

procedure TfrmACBrCIOT.ConfigurarEmail;
begin
  ACBrMail1.Host := edtSmtpHost.Text;
  ACBrMail1.Port := edtSmtpPort.Text;
  ACBrMail1.Username := edtSmtpUser.Text;
  ACBrMail1.Password := edtSmtpPass.Text;
  ACBrMail1.From := edtSmtpUser.Text;
  ACBrMail1.SetSSL := cbEmailSSL.Checked; // SSL - Conexao Segura
  ACBrMail1.SetTLS := cbEmailSSL.Checked; // Auto TLS
  ACBrMail1.ReadingConfirmation := False; // Pede confirmacao de leitura do email
  ACBrMail1.UseThread := False;           // Aguarda Envio do Email(nao usa thread)
  ACBrMail1.FromName := 'Projeto ACBr - ACBrCIOT';
end;

procedure TfrmACBrCIOT.LoadXML(RetWS: String; MyWebBrowser: TWebBrowser);
begin
  WriteToTXT(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml',
                      ConverteXMLtoUTF8(RetWS), False, False);

  MyWebBrowser.Navigate(PathWithDelim(ExtractFileDir(application.ExeName)) + 'temp.xml');
end;

procedure TfrmACBrCIOT.PathClick(Sender: TObject);
var
  Dir: string;
begin
  if Length(TEdit(Sender).Text) <= 0 then
     Dir := ExtractFileDir(application.ExeName)
  else
     Dir := TEdit(Sender).Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
    TEdit(Sender).Text := Dir;
end;

procedure TfrmACBrCIOT.sbPathEventoClick(Sender: TObject);
begin
  PathClick(edtPathEvento);
end;

procedure TfrmACBrCIOT.sbPathCIOTClick(Sender: TObject);
begin
  PathClick(edtPathCIOT);
end;

procedure TfrmACBrCIOT.sbtnCaminhoCertClick(Sender: TObject);
begin
  OpenDialog1.Title := 'Selecione o Certificado';
  OpenDialog1.DefaultExt := '*.pfx';
  OpenDialog1.Filter := 'Arquivos PFX (*.pfx)|*.pfx|Todos os Arquivos (*.*)|*.*';

  OpenDialog1.InitialDir := ExtractFileDir(application.ExeName);

  if OpenDialog1.Execute then
    edtCaminho.Text := OpenDialog1.FileName;
end;

procedure TfrmACBrCIOT.sbtnGetCertClick(Sender: TObject);
begin
  edtNumSerie.Text := ACBrCIOT1.SSL.SelecionarCertificado;
end;

procedure TfrmACBrCIOT.sbtnNumSerieClick(Sender: TObject);
var
  I: Integer;
  AddRow: Boolean;
begin
  ACBrCIOT1.SSL.LerCertificadosStore;
  AddRow := False;

  with frmSelecionarCertificado.StringGrid1 do
  begin
    ColWidths[0] := 220;
    ColWidths[1] := 250;
    ColWidths[2] := 120;
    ColWidths[3] := 80;
    ColWidths[4] := 150;

    Cells[0, 0] := 'Num.Série';
    Cells[1, 0] := 'Razão Social';
    Cells[2, 0] := 'CNPJ';
    Cells[3, 0] := 'Validade';
    Cells[4, 0] := 'Certificadora';
  end;

  for I := 0 to ACBrCIOT1.SSL.ListaCertificados.Count-1 do
  begin
    with ACBrCIOT1.SSL.ListaCertificados[I] do
    begin
      if (CNPJ <> '') then
      begin
        with frmSelecionarCertificado.StringGrid1 do
        begin
          if Addrow then
            RowCount := RowCount + 1;

          Cells[0, RowCount-1] := NumeroSerie;
          Cells[1, RowCount-1] := RazaoSocial;
          Cells[2, RowCount-1] := CNPJ;
          Cells[3, RowCount-1] := FormatDateBr(DataVenc);
          Cells[4, RowCount-1] := Certificadora;

          AddRow := True;
        end;
      end;
    end;
  end;

  frmSelecionarCertificado.ShowModal;

  if frmSelecionarCertificado.ModalResult = mrOK then
    edtNumSerie.Text := frmSelecionarCertificado.StringGrid1.Cells[0, frmSelecionarCertificado.StringGrid1.Row];
end;

procedure TfrmACBrCIOT.sbtnPathSalvarClick(Sender: TObject);
begin
  PathClick(edtPathLogs);
end;

procedure TfrmACBrCIOT.spPathSchemasClick(Sender: TObject);
begin
  PathClick(edtPathSchemas);
end;

end.
