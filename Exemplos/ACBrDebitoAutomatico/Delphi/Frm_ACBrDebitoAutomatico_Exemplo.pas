{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit Frm_ACBrDebitoAutomatico_Exemplo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, IniFiles, ExtCtrls,
  ACBrDebitoAutomatico;

type
  TfrmACBrDebitoAutomatico_Exemplo = class(TForm)
    btnGerar: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    lblColaborador: TLabel;
    lblPatrocinador: TLabel;
    lblDoar1: TLabel;
    lblDoar2: TLabel;
    gbConfiguracoes: TGroupBox;
    PageControl1: TPageControl;
    TabSheet4: TTabSheet;
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
    TabSheet2: TTabSheet;
    btnSalvarConfig: TBitBtn;
    Label30: TLabel;
    cbBanco: TComboBox;
    ckSalvar: TCheckBox;
    edtPathSalvar: TEdit;
    sbtnPathSalvar: TSpeedButton;
    LogMsg: TMemo;
    Label1: TLabel;
    btnLer: TButton;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    edtConvenio: TEdit;
    grpConta: TGroupBox;
    Label3: TLabel;
    edtAgencia: TEdit;
    Label4: TLabel;
    edtAgenciaDV: TEdit;
    Label5: TLabel;
    edtContaNumero: TEdit;
    Label6: TLabel;
    edtContaDV: TEdit;
    Label7: TLabel;
    edtDV: TEdit;
    Label8: TLabel;
    edtTipoConta: TEdit;
    ACBrDebitoAutomatico1: TACBrDebitoAutomatico;
    Label10: TLabel;
    cbLayoutVersao: TComboBox;

    procedure sbtnPathSalvarClick(Sender: TObject);
    procedure btnSalvarConfigClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnGerarClick(Sender: TObject);
    procedure lblColaboradorClick(Sender: TObject);
    procedure lblPatrocinadorClick(Sender: TObject);
    procedure lblDoar1Click(Sender: TObject);
    procedure lblDoar2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLerClick(Sender: TObject);
  private
    { Private declarations }
    procedure GravarConfiguracao;
    procedure LerConfiguracao;
    procedure ConfiguraComponente;
    procedure AlimentarComponente;
  public
    { Public declarations }
  end;

var
  frmACBrDebitoAutomatico_Exemplo: TfrmACBrDebitoAutomatico_Exemplo;

implementation

uses
 FileCtrl, DateUtils, TypInfo,
 ACBrUtil.FilesIO, ACBrUtil.Strings,
 ACBrDebitoAutomaticoConversao;

const
  SELDIRHELP = 1000;

{$R *.dfm}

procedure TfrmACBrDebitoAutomatico_Exemplo.GravarConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );

  try
    Ini.WriteString( 'Emitente', 'CNPJ'       , edtEmitCNPJ.Text);
    Ini.WriteString( 'Emitente', 'IE'         , edtEmitIE.Text);
    Ini.WriteString( 'Emitente', 'RazaoSocial', edtEmitRazao.Text);
    Ini.WriteString( 'Emitente', 'Fantasia'   , edtEmitFantasia.Text);
    Ini.WriteString( 'Emitente', 'Fone'       , edtEmitFone.Text);
    Ini.WriteString( 'Emitente', 'CEP'        , edtEmitCEP.Text);
    Ini.WriteString( 'Emitente', 'Logradouro' , edtEmitLogradouro.Text);
    Ini.WriteString( 'Emitente', 'Numero'     , edtEmitNumero.Text);
    Ini.WriteString( 'Emitente', 'Complemento', edtEmitComp.Text);
    Ini.WriteString( 'Emitente', 'Bairro'     , edtEmitBairro.Text);
    Ini.WriteString( 'Emitente', 'CodCidade'  , edtEmitCodCidade.Text);
    Ini.WriteString( 'Emitente', 'Cidade'     , edtEmitCidade.Text);
    Ini.WriteString( 'Emitente', 'UF'         , edtEmitUF.Text);

    Ini.WriteInteger( 'Banco', 'Banco'          , cbBanco.ItemIndex);
    Ini.WriteInteger( 'Banco', 'LayoutVersao'   , cbLayoutVersao.ItemIndex);
    Ini.WriteBool(    'Banco', 'Salvar'         , ckSalvar.Checked);
    Ini.WriteString(  'Banco', 'PathSalvar'     , edtPathSalvar.Text);

    Ini.WriteString( 'Conta', 'Convenio'   , edtConvenio.Text);
    Ini.WriteString( 'Conta', 'Agencia'    , edtAgencia.Text);
    Ini.WriteString( 'Conta', 'AgenciaDV'  , edtAgenciaDV.Text);
    Ini.WriteString( 'Conta', 'ContaNumero', edtContaNumero.Text);
    Ini.WriteString( 'Conta', 'ContaDV'    , edtContaDV.Text);
    Ini.WriteString( 'Conta', 'DV'         , edtDV.Text);
    Ini.WriteString( 'Conta', 'TipoConta'  , edtTipoConta.Text);
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.lblColaboradorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.lblDoar1Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.lblDoar2Click(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/14');
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.lblPatrocinadorClick(Sender: TObject);
begin
  OpenURL('http://acbr.sourceforge.net/drupal/?q=node/5');
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.LerConfiguracao;
var
  IniFile: String;
  Ini: TIniFile;
begin
  IniFile := ChangeFileExt( Application.ExeName, '.ini');

  Ini := TIniFile.Create( IniFile );

  try
    edtEmitCNPJ.Text       := Ini.ReadString( 'Emitente', 'CNPJ'       , '');
    edtEmitIE.Text         := Ini.ReadString( 'Emitente', 'IE'         , '');
    edtEmitRazao.Text      := Ini.ReadString( 'Emitente', 'RazaoSocial', '');
    edtEmitFantasia.Text   := Ini.ReadString( 'Emitente', 'Fantasia'   , '');
    edtEmitFone.Text       := Ini.ReadString( 'Emitente', 'Fone'       , '');
    edtEmitCEP.Text        := Ini.ReadString( 'Emitente', 'CEP'        , '');
    edtEmitLogradouro.Text := Ini.ReadString( 'Emitente', 'Logradouro' , '');
    edtEmitNumero.Text     := Ini.ReadString( 'Emitente', 'Numero'     , '');
    edtEmitComp.Text       := Ini.ReadString( 'Emitente', 'Complemento', '');
    edtEmitBairro.Text     := Ini.ReadString( 'Emitente', 'Bairro'     , '');
    edtEmitCodCidade.Text  := Ini.ReadString( 'Emitente', 'CodCidade'  , '');
    edtEmitCidade.Text     := Ini.ReadString( 'Emitente', 'Cidade'     , '');
    edtEmitUF.Text         := Ini.ReadString( 'Emitente', 'UF'         , '');

    cbBanco.ItemIndex        := Ini.ReadInteger('Banco', 'Banco'       , 0);
    cbLayoutVersao.ItemIndex := Ini.ReadInteger('Banco', 'LayoutVersao', 0);
    ckSalvar.Checked         := Ini.ReadBool(   'Banco', 'Salvar'      , True);
    edtPathSalvar.Text       := Ini.ReadString( 'Banco', 'PathSalvar'  , '');

    edtConvenio.Text    := Ini.ReadString( 'Conta', 'Convenio'   , '');
    edtAgencia.Text     := Ini.ReadString( 'Conta', 'Agencia'    , '');
    edtAgenciaDV.Text   := Ini.ReadString( 'Conta', 'AgenciaDV'  , '');
    edtContaNumero.Text := Ini.ReadString( 'Conta', 'ContaNumero', '');
    edtContaDV.Text     := Ini.ReadString( 'Conta', 'ContaDV'    , '');
    edtDV.Text          := Ini.ReadString( 'Conta', 'DV'         , '');
    edtTipoConta.Text   := Ini.ReadString( 'Conta', 'TipoConta'  , '');
  finally
    Ini.Free;
  end;
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.ConfiguraComponente;
begin
  ACBrDebitoAutomatico1.Configuracoes.Arquivos.Salvar           := ckSalvar.Checked;
  ACBrDebitoAutomatico1.Configuracoes.Arquivos.SepararPorCNPJ   := True;
  ACBrDebitoAutomatico1.Configuracoes.Arquivos.SepararPorMes    := True;
  ACBrDebitoAutomatico1.Configuracoes.Arquivos.AdicionarLiteral := True;
  ACBrDebitoAutomatico1.Configuracoes.Arquivos.PathSalvar       := edtPathSalvar.Text;

  with ACBrDebitoAutomatico1.Configuracoes.Geral do
  begin
    CNPJEmpresa := edtEmitCNPJ.Text;

    // A propriedade Banco deve ser a ultima a ser definida
    Banco := TBanco(cbBanco.ItemIndex);
    LayoutVersao := TDebitoLayoutVersao(cbLayoutVersao.ItemIndex);
  end;
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.sbtnPathSalvarClick(Sender: TObject);
var
  Dir : string;
begin
  if Length(edtPathSalvar.Text) <= 0 then
    Dir := ExtractFileDir(application.ExeName)
  else
    Dir := edtPathSalvar.Text;

  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt],SELDIRHELP) then
    edtPathSalvar.Text := Dir;
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.btnSalvarConfigClick(Sender: TObject);
begin
  GravarConfiguracao;
  ConfiguraComponente;
  LogMsg.Lines.Add('Novos de dados de Configuração Salvos.');
  LogMsg.Lines.Add('Componente Configurado com os novos dados.');
  LogMsg.Lines.Add(' ');
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.FormCreate(Sender: TObject);
var
  A: TBanco;
  B: TDebitoLayoutVersao;
begin
  cbBanco.Items.Clear;
  for A := Low(TBanco) to High(TBanco) do
    cbBanco.Items.Add(GetEnumName(TypeInfo(TBanco), integer(A)));
  cbBanco.ItemIndex := 0;

  cbLayoutVersao.Items.Clear;
  for B := Low(TDebitoLayoutVersao) to High(TDebitoLayoutVersao) do
    cbLayoutVersao.Items.Add(GetEnumName(TypeInfo(TDebitoLayoutVersao), integer(B)));
  cbLayoutVersao.ItemIndex := 0;
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.FormShow(Sender: TObject);
begin
  LerConfiguracao;
  ConfiguraComponente;
  LogMsg.Clear;
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.AlimentarComponente;
begin
  ACBrDebitoAutomatico1.Arquivos.Clear;

  with ACBrDebitoAutomatico1 do
  begin
    with Arquivos.New.DebitoAutomatico do
    begin
      //////////////////////////////////////////////////////////////////////////
      // Geral
      //////////////////////////////////////////////////////////////////////////

      Geral.Banco := Configuracoes.Geral.Banco;
      Geral.LayoutVersao := Configuracoes.Geral.LayoutVersao;

      //////////////////////////////////////////////////////////////////////////
      // Registro A
      //////////////////////////////////////////////////////////////////////////

      RegistroA.CodigoConvenio := edtConvenio.Text;
      RegistroA.NomeEmpresa := edtEmitRazao.Text;
      RegistroA.CodigoBanco := StrToIntDef(BancoToStr(Geral.Banco), 0);
      RegistroA.NomeBanco := BancoToDesc(Geral.Banco);
      RegistroA.Geracao := Now;
      // NSA - Numero Sequecial do Arquivo: 1, 2, 3, ...
      RegistroA.NSA := 1;
      RegistroA.LayoutVersao := Geral.LayoutVersao;

      // Registro C - Ocorrências no Cancelamento do Débito Automático
      with RegistroC.New do
      begin
        IdentificacaoClienteEmpresa := '123';
        AgenciaDebito := '0001';
        IdentificacaoClienteBanco := 'ABCD';
        Ocorrencia1 := 'ABC';
        Ocorrencia2 := 'XYZ';
        CodigoMovimento := mcExclusao;
      end;

      with RegistroC.New do
      begin
        IdentificacaoClienteEmpresa := '345';
        AgenciaDebito := '0002';
        IdentificacaoClienteBanco := 'EFGH';
        Ocorrencia1 := 'ABC';
        Ocorrencia2 := 'XYZ';
        CodigoMovimento := mcExclusao;
      end;

      with RegistroD.New do
      begin
        IdentificacaoClienteEmpresaAnterior := '123';
        AgenciaDebito := '0001';
        IdentificacaoClienteBanco := 'ABCD';
        IdentificacaoClienteEmpresaAtual := 'A123';

        Ocorrencia := 'Ocorrencia';

        if Configuracoes.Geral.LayoutVersao = lv8 then
        begin
          NovaDataVencAutorizacao := Now;
          AlteracaoOpcaoUsoChequeEspecial := aoSim;
          AlteracaoOpcaoDebParcialIntegralPosVenc := aoSim;
        end;
        CodigoMovimento := maAlteracao;
      end;

      with RegistroE.New do
      begin
        IdentificacaoClienteEmpresa := '123';
        AgenciaDebito := '0001';
        IdentificacaoClienteBanco := 'ABCD';
        Vencimento := StrToDate('15/06/2023');
        DebitoMoeda := mREAL;
        Valor := 100;
        UsoEmpresa := 'Uso da Empresa';
        UsoEmpresaXY := ueX;
        UsoEmpresaValorTotalTributos := 0;
        TipoIdentificacao := cCNPJ;
        CPFCNPJ := '01234567000123';
        TipoOperacao := toArredondamentoMercantil;
        UtilizacaoChequeEspecial := snNao;
        OpcaoDebParcialIntegralPosVenc := snNao;
        CodigoMovimento := mdDebitoNormal;
      end;

      with RegistroI.New do
      begin
        IdentificacaoClienteEmpresa := '123';
        TipoIdentificacao := cCNPJ;
        CPFCNPJ := '01234567000123';
        Nome := 'Nome do Cliente';
        Cidade := 'Cidade do Cliente';
        UF := 'SP';
      end;

      with RegistroJ.New do
      begin
        NSA := 1;
        Geracao := Now;
        TotalRegistros := 1;
        ValorTotal := 100;
        Processamento := Now;
      end;

      with RegistroK.New do
      begin
        IdentificacaoClienteEmpresa := '123';
        AgenciaDebito := '0001';
        IdentificacaoClienteBanco := '123';
        TipoTratamento := 1;
        TipoIdentificacao := cCNPJ;
        CPFCNPJ := '01234567000123';
      end;

      with RegistroL.New do
      begin
        FaturamentoContas := Now;
        VencimentoFatura := Now;
        RemessaArquivoBanco := Now;
        RemessaContasFisicas := Now;
      end;
    end;
  end;
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.btnGerarClick(Sender: TObject);
var
  NomeArquivo: String;
begin
  ForceDirectories(edtPathSalvar.Text);

  AlimentarComponente;

  NomeArquivo := edtPathSalvar.Text + '\Deb'+FormatDateTime('DDMM', now) + '-' +
                 BancoToStr(ACBrDebitoAutomatico1.Configuracoes.Geral.Banco) +
                 '.seq';

  ACBrDebitoAutomatico1.GravarTxtRemessa(NomeArquivo);

  LogMsg.Lines.Add('Arquivo de Remessa: ' + NomeArquivo);
  LogMsg.Lines.Add('Gerado com sucesso.');
  LogMsg.Lines.Add(' ');
end;

procedure TfrmACBrDebitoAutomatico_Exemplo.btnLerClick(Sender: TObject);
var
  NomeArquivo: String;
  i, k: Integer;
begin
  OpenDialog1.Title := 'Selecione o Arquivo de Retorno';
  OpenDialog1.DefaultExt := '*.txt';
  OpenDialog1.Filter :=
    'Arquivos TXT (*.txt)|*.txt|Todos os Arquivos (*.*)|*.*';
  OpenDialog1.InitialDir := ACBrDebitoAutomatico1.Configuracoes.Arquivos.PathSalvar;

  if OpenDialog1.Execute then
  begin
    NomeArquivo := OpenDialog1.FileName;

    ACBrDebitoAutomatico1.LerTxtRetorno(NomeArquivo);

    LogMsg.Lines.Add('Arquivo de Retorno: ' + NomeArquivo);
    LogMsg.Lines.Add('Lido com sucesso.');
    LogMsg.Lines.Add(' ');

    if ACBrDebitoAutomatico1.Arquivos.Count > 0 then
      LogMsg.Lines.Add('Lista de Ocorrências:')
    else
      LogMsg.Lines.Add('Sem nenhuma Ocorrência.');

    for i := 0 to ACBrDebitoAutomatico1.Arquivos.Count -1 do
    begin
      for k := 0 to ACBrDebitoAutomatico1.Arquivos.Items[i].DebitoAutomatico.RegistroA.Aviso.Count -1 do
      begin
        with ACBrDebitoAutomatico1.Arquivos.Items[i].DebitoAutomatico.RegistroA.Aviso.Items[k] do
        begin
          LogMsg.Lines.Add('Código........:' + CodigoRetorno);
          LogMsg.Lines.Add('Mensagem......:' + MensagemRetorno);
          LogMsg.Lines.Add('Segmento......:' + Segmento);
          LogMsg.Lines.Add('Identificação.:' + Identificacao);
        end;
      end;
    end;
  end;
end;

end.
