{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Daniel Simoes de Almeida             }
{                                         Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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

{******************************************************************************
|* Historico
|*
|* 29/03/2012: Isaque Pinheiro / Régys Borges da Silveira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}
unit uPrincipal;

interface

uses
  Windows, Messages, FileCtrl, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, pngimage,
  uFrameLista, IOUtils, UITypes,
  JclIDEUtils, JclCompilerUtils, ACBrUtil,
  Types, JvComponentBase, JvCreateProcess, JvExControls, JvAnimatedImage,
  JvGIFCtrl, JvWizard, JvWizardRouteMapNodes, CheckLst,
  ACBrInstallDelphiComponentes;

type

  TfrmPrincipal = class(TForm)
    wizPrincipal: TJvWizard;
    wizMapa: TJvWizardRouteMapNodes;
    wizPgConfiguracao: TJvWizardInteriorPage;
    wizPgInstalacao: TJvWizardInteriorPage;
    wizPgFinalizar: TJvWizardInteriorPage;
    wizPgInicio: TJvWizardWelcomePage;
    Label2: TLabel;
    edtDirDestino: TEdit;
    Label6: TLabel;
    imgLogomarca: TImage;
    lstMsgInstalacao: TListBox;
    pnlTopo: TPanel;
    Label9: TLabel;
    btnSelecDirInstall: TSpeedButton;
    imgGifPropagandaACBrSAC: TJvGIFAnimator;
    Label3: TLabel;
    pgbInstalacao: TProgressBar;
    lblUrlACBrSac1: TLabel;
    lblUrlForum1: TLabel;
    lblUrlACBr1: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label7: TLabel;
    btnInstalarACBr: TSpeedButton;
    btnVisualizarLogCompilacao: TSpeedButton;
    pnlInfoCompilador: TPanel;
    wizPgPacotes: TJvWizardInteriorPage;
    rdgDLL: TRadioGroup;
    ckbCopiarTodasDll: TCheckBox;
    ckbBCB: TCheckBox;
    lbInfo: TListBox;
    Label8: TLabel;
    chkDeixarSomenteLIB: TCheckBox;
    ckbRemoverArquivosAntigos: TCheckBox;
    JvCreateProcess1: TJvCreateProcess;
    Label22: TLabel;
    ckbRemoveOpenSSL: TCheckBox;
    ckbRemoveCapicom: TCheckBox;
    ckbCargaDllTardia: TCheckBox;
    ckbRemoverCastWarnings: TCheckBox;
    ckbUsarArquivoConfig: TCheckBox;
    framePacotes1: TframePacotes;
    ckbRemoveXMLSec: TCheckBox;
    wizPgSelectIDEs: TJvWizardInteriorPage;
    Label23: TLabel;
    edtDelphiVersion: TComboBox;
    clbDelphiVersion: TCheckListBox;

    procedure imgPropaganda1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure wizPgInicioNextButtonClick(Sender: TObject; var Stop: Boolean);
    procedure URLClick(Sender: TObject);
    procedure btnSelecDirInstallClick(Sender: TObject);
    procedure wizPrincipalCancelButtonClick(Sender: TObject);
    procedure wizPrincipalFinishButtonClick(Sender: TObject);
    procedure wizPgConfiguracaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnInstalarACBrClick(Sender: TObject);
    procedure wizPgInstalacaoNextButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure btnVisualizarLogCompilacaoClick(Sender: TObject);
    procedure wizPgInstalacaoEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure clbDelphiVersionClick(Sender: TObject);
    procedure wizPgSelectIDEsNextButtonClick(Sender: TObject; var Stop: Boolean);
  private
    oACBr: TJclBorRADToolInstallations;
    iVersion: Integer;
    sPlatform: string;
    tPlatform: TJclBDSPlatform;
    sDirLibrary: string;
    FPacoteAtual: TFileName;
    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure OutputCallLine(const Text: string);
    procedure GravarConfiguracoesEmArquivoIni;
    procedure LerConfiguracoes;
    function PathApp: String;
    function PathArquivoIni: String;
    function PathArquivoLog(const NomeVersao: string): String;
    procedure MostraDadosVersao(const PastaACBr: string);
    function GetPathACBrInc: TFileName;
    procedure ValidarSeExistemPacotesNasPastas(var Stop: Boolean; const PastaACBr: string; ListaPacotes:
        TPacotes);
    procedure IncrementaBarraProgresso;
    procedure Logar(const AString: String);
    function ProcedeInstalacao: Boolean;
    procedure CompilarPacotes(InstalacaoAtual: TJclBorRADToolInstallation; var FCountErros: Integer; const
        PastaACBr:string; listaPacotes: TPacotes);
    procedure InstalarPacotes(InstalacaoAtual: TJclBorRADToolInstallation; var FCountErros: Integer; const
        PastaACBr: string; listaPacotes: TPacotes);
    procedure AbrirArquivoLogAtual;
    function VersionNumberToNome(const AVersionStr: string): string;
  public

  end;

var
  frmPrincipal: TfrmPrincipal;

implementation

uses
  ShellApi, IniFiles, StrUtils, Math, Registry, ACBrInstallUtils;

{$R *.dfm}

// retornar o path do aplicativo
function TfrmPrincipal.PathApp: String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
end;

// retornar o caminho completo para o arquivo .ini de configurações
function TfrmPrincipal.PathArquivoIni: String;
begin
  Result := ChangeFileExt(ParamStr(0), '.ini');
end;

// retornar o caminho completo para o arquivo de logs
function TfrmPrincipal.PathArquivoLog(const NomeVersao: string): String;
begin
  Result := PathApp + 'log_' + StringReplace(NomeVersao, ' ', '_', [rfReplaceAll]) + '.txt';
end;

procedure TfrmPrincipal.ValidarSeExistemPacotesNasPastas(var Stop: Boolean; const PastaACBr: string;
      ListaPacotes: TPacotes);
var
  I: Integer;
  NomePacote: string;
  sDirPackage: string;
begin
  // verificar se os pacotes existem antes de seguir para o próximo passo
  for I := 0 to ListaPacotes.Count - 1 do
  begin
    if ListaPacotes[I].Checked then
    begin
      NomePacote := ListaPacotes[I].Caption;
      // Busca diretório do pacote
      sDirPackage := FindDirPackage(IncludeTrailingPathDelimiter(PastaACBr) + 'Pacotes\Delphi', NomePacote);
      if Trim(sDirPackage) = '' then
        raise Exception.Create('Não foi possível retornar o diretório do pacote : ' + NomePacote);
      if IsDelphiPackage(NomePacote) then
      begin
        if not FileExists(IncludeTrailingPathDelimiter(sDirPackage) + NomePacote) then
        begin
          Stop := True;
          Application.MessageBox(PWideChar(Format('Pacote "%s" não encontrado, efetue novamente o download do repositório', [NomePacote])), 'Erro.', MB_ICONERROR + MB_OK);
          Break;
        end;
      end;
    end;
  end;
end;

// ler o arquivo .ini de configurações e setar os campos com os valores lidos
procedure TfrmPrincipal.LerConfiguracoes;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    edtDirDestino.Text             := ArqIni.ReadString('CONFIG', 'DiretorioInstalacao', ExtractFilePath(ParamStr(0)));
    rdgDLL.ItemIndex               := ArqIni.ReadInteger('CONFIG','DestinoDLL', 0);
    ckbCopiarTodasDll.Checked      := True;
    ckbBCB.Checked                 := ArqIni.ReadBool('CONFIG','C++Builder', False);
    chkDeixarSomenteLIB.Checked    := ArqIni.ReadBool('CONFIG','DexarSomenteLib', False);
    ckbRemoveOpenSSL.Checked       := ArqIni.ReadBool('CONFIG','RemoveOpenSSL', False);
    ckbRemoveCapicom.Checked       := ArqIni.ReadBool('CONFIG','RemoveCapicom', False);
    ckbRemoveXMLSec.Checked        := True;
    ckbCargaDllTardia.Checked      := ArqIni.ReadBool('CONFIG','CargaDllTardia', False);
    ckbRemoverCastWarnings.Checked := ArqIni.ReadBool('CONFIG','RemoverCastWarnings', False);
    ckbUsarArquivoConfig.Checked   := True;

    for I := 0 to framePacotes1.Pacotes.Count - 1 do
      framePacotes1.Pacotes[I].Checked := ArqIni.ReadBool('PACOTES', framePacotes1.Pacotes[I].Caption, False);
  finally
    ArqIni.Free;
  end;
end;

procedure TfrmPrincipal.MostraDadosVersao(const PastaACBr: string);
var
  Cabecalho: string;
begin
  Cabecalho := 'Versão do delphi: ' + edtDelphiVersion.Text + ' (' + IntToStr(iVersion) + ')' + sPlatform + '(' + IntToStr(Integer(tPlatform)) + ')' + sLineBreak +
               'Dir. Instalação : ' + PastaACBr + sLineBreak +
               'Dir. Bibliotecas: ' + sDirLibrary;

  WriteToTXT(PathArquivoLog(edtDelphiVersion.Text), Cabecalho + sLineBreak, False);

  // mostrar ao usuário as informações de compilação
  lbInfo.Clear;
  lbInfo.Items.Text := Cabecalho;
end;

// gravar as configurações efetuadas pelo usuário
procedure TfrmPrincipal.GravarConfiguracoesEmArquivoIni;
var
  ArqIni: TIniFile;
  I: Integer;
begin
  ArqIni := TIniFile.Create(PathArquivoIni);
  try
    ArqIni.WriteString('CONFIG', 'DiretorioInstalacao', edtDirDestino.Text);
    ArqIni.WriteInteger('CONFIG','DestinoDLL', rdgDLL.ItemIndex);
    ArqIni.WriteBool('CONFIG','C++Builder',ckbBCB.Checked);
    ArqIni.WriteBool('CONFIG','DexarSomenteLib', chkDeixarSomenteLIB.Checked);
    ArqIni.WriteBool('CONFIG','RemoveOpenSSL', ckbRemoveOpenSSL.Checked);
    ArqIni.WriteBool('CONFIG','RemoveCapicom', ckbRemoveCapicom.Checked);
    ArqIni.WriteBool('CONFIG','RemoveXmlSec', ckbRemoveXMLSec.Checked);
    ArqIni.WriteBool('CONFIG','CargaDllTardia', ckbCargaDllTardia.Checked);
    ArqIni.WriteBool('CONFIG','RemoverCastWarnings', ckbRemoverCastWarnings.Checked);

    for I := 0 to framePacotes1.Pacotes.Count - 1 do
      ArqIni.WriteBool('PACOTES', framePacotes1.Pacotes[I].Caption, framePacotes1.Pacotes[I].Checked);
  finally
    ArqIni.Free;
  end;
end;

// Evento disparado a cada ação do instalador
procedure TfrmPrincipal.OutputCallLine(const Text: string);
begin
  // remover a warnings de conversão de string (delphi 2010 em diante)
  // as diretivas -W e -H não removem estas mensagens
  if (pos('Warning: W1057', Text) <= 0) and ((pos('Warning: W1058', Text) <= 0)) then
    WriteToTXT(PathArquivoLog(edtDelphiVersion.Text), Text);
end;

// evento para setar os parâmetros do compilador antes de compilar
procedure TfrmPrincipal.BeforeExecute(Sender: TJclBorlandCommandLineTool);
var
  LArquivoCfg: TFilename;
begin
  // limpar os parâmetros do compilador
  Sender.Options.Clear;

  // não utilizar o dcc32.cfg
  if (oACBr.Installations[iVersion].SupportsNoConfig) and
     // -- Arquivo cfg agora opcional no caso de paths muito extensos
     (not ckbUsarArquivoConfig.Checked) then
    Sender.Options.Add('--no-config');

  // -B = Build all units
  Sender.Options.Add('-B');
  // O+ = Optimization
  Sender.Options.Add('-$O-');
  // W- = Generate stack frames
  Sender.Options.Add('-$W+');
  // Y+ = Symbol reference info
  Sender.Options.Add('-$Y-');
  // -M = Make modified units
  Sender.Options.Add('-M');
  // -Q = Quiet compile
  Sender.Options.Add('-Q');
  // não mostrar warnings
  Sender.Options.Add('-H-');
  // não mostrar hints
  Sender.Options.Add('-W-');
  // -D<syms> = Define conditionals
  Sender.Options.Add('-DRELEASE');
  // -U<paths> = Unit directories
  Sender.AddPathOption('U', oACBr.Installations[iVersion].LibFolderName[tPlatform]);
  Sender.AddPathOption('U', oACBr.Installations[iVersion].LibrarySearchPath[tPlatform]);
  Sender.AddPathOption('U', sDirLibrary);
  // -I<paths> = Include directories
  Sender.AddPathOption('I', oACBr.Installations[iVersion].LibrarySearchPath[tPlatform]);
  // -R<paths> = Resource directories
  Sender.AddPathOption('R', oACBr.Installations[iVersion].LibrarySearchPath[tPlatform]);
  // -N0<path> = unit .dcu output directory
  Sender.AddPathOption('N0', sDirLibrary);
  Sender.AddPathOption('LE', sDirLibrary);
  Sender.AddPathOption('LN', sDirLibrary);

  // ************ C++ Builder *************** //
  if ckbBCB.Checked then
  begin
     // -JL compila c++ builder
     Sender.AddPathOption('JL', sDirLibrary);
     // -NO compila .dpi output directory c++ builder
     Sender.AddPathOption('NO', sDirLibrary);
     // -NB compila .lib output directory c++ builder
     Sender.AddPathOption('NB', sDirLibrary);
     // -NH compila .hpp output directory c++ builder
     Sender.AddPathOption('NH', sDirLibrary);
  end;
  //
  with oACBr.Installations[iVersion] do
  begin
     // -- Path para instalar os pacotes do Rave no D7, nas demais versões
     // -- o path existe.
     if VersionNumberStr = 'd7' then
        Sender.AddPathOption('U', oACBr.Installations[iVersion].RootDir + '\Rave5\Lib');

     // -- Na versão XE2 por motivo da nova tecnologia FireMonkey, deve-se adicionar
     // -- os prefixos dos nomes, para identificar se será compilado para VCL ou FMX
     if VersionNumberStr = 'd16' then
        Sender.Options.Add('-NSData.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win');

     if MatchText(VersionNumberStr, ['d17','d18','d19','d20','d21','d22','d23','d24','d25','d26']) then
        Sender.Options.Add('-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell');

  end;
  
  if (ckbUsarArquivoConfig.Checked) then
  begin
    LArquivoCfg := ChangeFileExt(FPacoteAtual, '.cfg');
    Sender.Options.SaveToFile(LArquivoCfg);
    Sender.Options.Clear;
  end;
end;

function TfrmPrincipal.VersionNumberToNome(const AVersionStr: string): string;
begin
  if      AVersionStr = 'd3' then
    Result := 'Delphi 3'
  else if AVersionStr = 'd4' then
    Result := 'Delphi 4'
  else if AVersionStr = 'd5' then
    Result := 'Delphi 5'
  else if AVersionStr = 'd6' then
    Result := 'Delphi 6'
  else if AVersionStr = 'd7' then
    Result := 'Delphi 7'
  else if AVersionStr = 'd9' then
    Result := 'Delphi 2005'
  else if AVersionStr = 'd10' then
    Result := 'Delphi 2006'
  else if AVersionStr = 'd11' then
    Result := 'Delphi 2007'
  else if AVersionStr = 'd12' then
    Result := 'Delphi 2009'
  else if AVersionStr = 'd14' then
    Result := 'Delphi 2010'
  else if AVersionStr = 'd15' then
    Result := 'Delphi XE'
  else if AVersionStr = 'd16' then
    Result := 'Delphi XE2'
  else if AVersionStr = 'd17' then
    Result := 'Delphi XE3'
  else if AVersionStr = 'd18' then
    Result := 'Delphi XE4'
  else if AVersionStr = 'd19' then
    Result := 'Delphi XE5'
  else if AVersionStr = 'd20' then
    Result := 'Delphi XE6'
  else if AVersionStr = 'd21' then
    Result := 'Delphi XE7'
  else if AVersionStr = 'd22' then
    Result := 'Delphi XE8'
  else if AVersionStr = 'd23' then
    Result := 'Delphi 10 Seattle'
  else if AVersionStr = 'd24' then
    Result := 'Delphi 10.1 Berlin'
  else if AVersionStr = 'd25' then
    Result := 'Delphi 10.2 Tokyo'
  else if AVersionStr = 'd26' then
    Result := 'Delphi 10.3 Rio'
  else
    Result := '';
end;
procedure TfrmPrincipal.FormCreate(Sender: TObject);
var
  iFor: Integer;
begin
  iVersion    := -1;
  sDirLibrary := '';
  sPlatform   := 'Win32';

  oACBr := TJclBorRADToolInstallations.Create;

  // popular o combobox de versões do delphi instaladas na máquina
  for iFor := 0 to oACBr.Count - 1 do
  begin
    clbDelphiVersion.Items.Add(VersionNumberToNome(oACBr.Installations[iFor].VersionNumberStr));
    //Desabilitar versões não suportadas
    clbDelphiVersion.ItemEnabled[iFor] := MatchText(oACBr.Installations[iFor].VersionNumberStr, ['d3','d4','d5','d6']);
  end;

  edtDelphiVersion.Items.Text := clbDelphiVersion.Items.Text;

  if edtDelphiVersion.Items.Count > 0 then
  begin
    edtDelphiVersion.ItemIndex := 0;
    iVersion := 0;
  end;

  LerConfiguracoes;
end;

procedure TfrmPrincipal.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  oACBr.Free;
end;

function TfrmPrincipal.GetPathACBrInc: TFileName;
begin
  Result := IncludeTrailingPathDelimiter(edtDirDestino.Text) + 'Fontes\ACBrComum\ACBr.inc';
end;

procedure TfrmPrincipal.Logar(const AString: String);
begin
  lstMsgInstalacao.Items.Add(AString);
  lstMsgInstalacao.ItemIndex := lstMsgInstalacao.Count - 1;
  Application.ProcessMessages;

  WriteToTXT(PathArquivoLog(edtDelphiVersion.Text), AString);
end;

function TfrmPrincipal.ProcedeInstalacao: Boolean;
var
  ACBrInstaladorAux: TACBrInstallComponentes;
  InstalacaoAtual: TJclBorRADToolInstallation;
  iListaVer: Integer;
  FCountErros: Integer;
  sDestino   : TDestino;
  JaCopiouDLLs: Boolean;
  ListaPacotes: TPacotes;
  sDirRoot: string;
begin
  Result := False;
  JaCopiouDLLs := False;

  case rdgdll.ItemIndex of
    0 : sDestino := tdSystem;
    1 : sDestino := tdDelphi;
    2 : sDestino := tdNone;
  else
    sDestino := tdNone;
  end;

  ListaPacotes := framePacotes1.Pacotes;

  ACBrInstaladorAux := TACBrInstallComponentes.Create(Application);
  try
    ACBrInstaladorAux.OnProgresso       := IncrementaBarraProgresso;
    ACBrInstaladorAux.OnInformaSituacao := Logar;

    ACBrInstaladorAux.Opcoes.LimparArquivosACBrAntigos := ((ckbRemoverArquivosAntigos.Checked) and
                                  (Application.MessageBox('Você optou por limpar arquivos antigos do ACBr do seu computador, essa ação pode demorar vários minutos, deseja realmente continuar com está ação?',
                                                          'Limpar', MB_YESNO + MB_DEFBUTTON2) = ID_YES)
                                 );
    ACBrInstaladorAux.Opcoes.DeixarSomentePastasLib    := chkDeixarSomenteLIB.Checked;
    ACBrInstaladorAux.Opcoes.DeveInstalarCapicom       := not ckbRemoveCapicom.Checked;
    ACBrInstaladorAux.Opcoes.DeveInstalarOpenSSL       := not ckbRemoveOpenSSL.Checked;
    ACBrInstaladorAux.Opcoes.DeveCopiarOutrasDLLs      := ckbCopiarTodasDll.Checked;
    ACBrInstaladorAux.Opcoes.DeveInstalarXMLSec        := not ckbRemoveXMLSec.Checked;
    ACBrInstaladorAux.Opcoes.UsarCargaTardiaDLL        := ckbCargaDllTardia.Checked;
    ACBrInstaladorAux.Opcoes.RemoverStringCastWarnings := ckbRemoverCastWarnings.Checked;
    ACBrInstaladorAux.Opcoes.UsarCpp                   := ckbBCB.Checked;

    ACBrInstaladorAux.DesligarDefines(GetPathACBrInc);

    for iListaVer := 0 to oACBr.Count - 1 do
    begin
      // só instala as versão marcadas para instalar.
      if (not clbDelphiVersion.Checked[iListaVer]) then
      begin
        Continue
      end;
      // seleciona a versão no combobox.
      edtDelphiVersion.ItemIndex := iListaVer;
      iVersion := iListaVer;

      // define dados da plataforna selecionada
      sDirRoot := IncludeTrailingPathDelimiter(edtDirDestino.Text);
      sDirLibrary := sDirRoot + 'Lib\Delphi\Lib' + AnsiUpperCase(oACBr.Installations[iVersion].VersionNumberStr);
    //  if edtPlatform.ItemIndex = 0 then // Win32
    //  begin
        tPlatform   := bpWin32;
        sPlatform   := 'Win32';
    //  end
    //  else
    //  if edtPlatform.ItemIndex = 1 then // Win64
    //  begin
    //    tPlatform   := bpWin64;
    //    sDirLibrary := sDirLibrary + 'x64';
    //  end;
      // limpar o log
      lstMsgInstalacao.Clear;
      // setar barra de progresso
      pgbInstalacao.Position := 0;
      pgbInstalacao.Max := (ListaPacotes.Count * 2) + 6;

      FCountErros := 0;
      MostraDadosVersao(sDirRoot);

      InstalacaoAtual := oACBr.Installations[iVersion];

      // -- Evento disparado antes de iniciar a execução do processo.
      InstalacaoAtual.DCC32.OnBeforeExecute := BeforeExecute;
      // -- Evento para saidas de mensagens.
      InstalacaoAtual.OutputCallback := OutputCallLine;

      ACBrInstaladorAux.FazInstalacaoInicial(InstalacaoAtual, tPlatform, sDirRoot, sDirLibrary);

      // *************************************************************************
      // compilar os pacotes primeiramente
      // *************************************************************************
      Logar(sLineBreak+'COMPILANDO OS PACOTES...');
      CompilarPacotes(InstalacaoAtual, FCountErros, sDirRoot, ListaPacotes);

      // *************************************************************************
      // instalar os pacotes somente se não ocorreu erro na compilação e plataforma for Win32
      // *************************************************************************
      if FCountErros > 0 then
      begin
        Logar('Abortando... Ocorreram erros na compilação dos pacotes.');
        Break;
      end;

      if ( tPlatform = bpWin32) then
      begin
        Logar(sLineBreak+'INSTALANDO OS PACOTES...');
        InstalarPacotes(InstalacaoAtual, FCountErros, sDirRoot, ListaPacotes);
      end
      else
      begin
        Logar('Para a plataforma de 64 bits os pacotes são somente compilados.');
      end;

      ACBrInstaladorAux.InstalarOutrosRequisitos(InstalacaoAtual, tPlatform, sDirRoot, sDirLibrary);

      if (FCountErros = 0) then
      begin
        if (sDestino = tdDelphi) or (not JaCopiouDLLs) then
        begin
          ACBrInstaladorAux.FazInstalacaoDLLs(FCountErros, sDestino, sDirRoot, IncludeTrailingPathDelimiter(InstalacaoAtual.BinFolderName));
          JaCopiouDLLs := True;
        end;
      end;

    end;

  finally
    ACBrInstaladorAux.Free;
  end;

  if FCountErros = 0 then
  begin
    Result := True;
    Application.MessageBox(PWideChar('Pacotes compilados e instalados com sucesso! ' + sLineBreak +
                                     'Clique em "Próximo" para finalizar a instalação.'),
                           'Instalação', MB_ICONINFORMATION + MB_OK);
  end
  else if FCountErros > 0 then
  begin
    if Application.MessageBox(PWideChar('Ocorreram erros durante o processo de instalação, ' + sLineBreak +
                                        'para maiores informações verifique o arquivo de log gerado.' +
                                        sLineBreak + sLineBreak +
                                        'Deseja visualizar o arquivo de log gerado?'),
                              'Instalação', MB_ICONQUESTION + MB_YESNO) = ID_YES then
    begin
      btnVisualizarLogCompilacao.Click;
    end;
  end;

end;

procedure TfrmPrincipal.CompilarPacotes(InstalacaoAtual: TJclBorRADToolInstallation; var FCountErros: Integer;
      const PastaACBr: string; listaPacotes: TPacotes);
var
  iDpk: Integer;
  NomePacote: string;
  sDirPackage: string;
begin
  for iDpk := 0 to listaPacotes.Count - 1 do
  begin
    if (not listaPacotes[iDpk].Checked) then
    begin
      IncrementaBarraProgresso;
      Continue;
    end;

    NomePacote := listaPacotes[iDpk].Caption;
    // Busca diretório do pacote
    sDirPackage := FindDirPackage(IncludeTrailingPathDelimiter(PastaACBr) + 'Pacotes\Delphi', NomePacote);
    if (IsDelphiPackage(NomePacote)) then
    begin
      WriteToTXT(PathArquivoLog(edtDelphiVersion.Text), '');
      FPacoteAtual := sDirPackage + NomePacote;
      if InstalacaoAtual.CompilePackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
        Logar(Format('Pacote "%s" compilado com sucesso.', [NomePacote]))
      else
      begin
        Inc(FCountErros);
        Logar(Format('Erro ao compilar o pacote "%s".', [NomePacote]));
        // parar no primeiro erro para evitar de compilar outros pacotes que
        // precisam do pacote que deu erro
        Break;
      end;
    end;
    IncrementaBarraProgresso;
  end;
end;

procedure TfrmPrincipal.InstalarPacotes(InstalacaoAtual: TJclBorRADToolInstallation; var FCountErros: Integer;
      const PastaACBr: string; listaPacotes: TPacotes);
var
  iDpk: Integer;
  NomePacote: string;
  bRunOnly: Boolean;
  sDirPackage: string;
begin
  for iDpk := 0 to listaPacotes.Count - 1 do
  begin
    NomePacote := listaPacotes[iDpk].Caption;
    // Busca diretório do pacote
    sDirPackage := FindDirPackage(IncludeTrailingPathDelimiter(PastaACBr) + 'Pacotes\Delphi', NomePacote);
    if IsDelphiPackage(NomePacote) then
    begin
      FPacoteAtual := sDirPackage + NomePacote;
      // instalar somente os pacotes de designtime
      GetDPKFileInfo(sDirPackage + NomePacote, bRunOnly);
      if not bRunOnly then
      begin
        // se o pacote estiver marcado instalar, senão desinstalar
        if listaPacotes[iDpk].Checked then
        begin
          if InstalacaoAtual.InstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
            Logar(Format('Pacote "%s" instalado com sucesso.', [NomePacote]))
          else
          begin
            Inc(FCountErros);
            Logar(Format('Ocorreu um erro ao instalar o pacote "%s".', [NomePacote]));
            Break;
          end;
        end
        else
        begin
          if InstalacaoAtual.UninstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
            Logar(Format('Pacote "%s" removido com sucesso...', [NomePacote]));
        end;
      end;
    end;
    IncrementaBarraProgresso;
  end;
end;

procedure TfrmPrincipal.AbrirArquivoLogAtual;
begin
  ShellExecute(Handle, 'open', PWideChar(PathArquivoLog(edtDelphiVersion.Text)), '', '', 1);
end;

procedure TfrmPrincipal.IncrementaBarraProgresso;
begin
  pgbInstalacao.Position := pgbInstalacao.Position + 1;
  Application.ProcessMessages;
end;

// botão de compilação e instalação dos pacotes selecionados no treeview
procedure TfrmPrincipal.btnInstalarACBrClick(Sender: TObject);
var
  Instalou: Boolean;
begin
  Instalou := False;
  btnInstalarACBr.Enabled := False;
  wizPgInstalacao.EnableButton(bkBack, False);
  wizPgInstalacao.EnableButton(bkNext, False);
  wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), False);
  try
    Instalou := ProcedeInstalacao;
  finally
    btnInstalarACBr.Enabled := True;
    wizPgInstalacao.EnableButton(bkBack, True);
    wizPgInstalacao.EnableButton(bkNext, Instalou);
    wizPgInstalacao.EnableButton(TJvWizardButtonKind(bkCancel), True);
  end;
end;

// chama a caixa de dialogo para selecionar o diretório de instalação
// seria bom que a caixa fosse aquele que possui o botão de criar pasta
procedure TfrmPrincipal.btnSelecDirInstallClick(Sender: TObject);
var
  Dir: String;
begin
  if SelectDirectory('Selecione o diretório de instalação', '', Dir, [sdNewFolder, sdNewUI, sdValidateDir]) then
    edtDirDestino.Text := Dir;
end;

// quando trocar a versão verificar se libera ou não o combo
// da plataforma de compilação
procedure TfrmPrincipal.clbDelphiVersionClick(Sender: TObject);
begin
  if clbDelphiVersion.ItemIndex < 0 then
  begin
    Exit
  end;

  if MatchText(oACBr.Installations[clbDelphiVersion.ItemIndex].VersionNumberStr, ['d7','d9','d10','d11']) then
  begin
    Application.MessageBox(
      'Atenção: Embora o ACBr continue suportando versões anteriores do Delphi, incentivamos que você atualize o quanto antes para versões mais recentes do Delphi ou considere migrar para o Lazarus.',
      'Erro.',
      MB_OK + MB_ICONWARNING
    );
  end;

  // C++ Builder a partir do D2006, versões anteriores tem IDE independentes.
  ckbBCB.Enabled := MatchText(oACBr.Installations[iVersion].VersionNumberStr, ['d10','d11','d12','d14','d15','d16','d17','d18','d19','d20','d21','d22','d23','d24','d25','d26']);
  if not ckbBCB.Enabled then
     ckbBCB.Checked := False;
end;

// abrir o endereço do ACBrSAC quando clicar na propaganda
procedure TfrmPrincipal.imgPropaganda1Click(Sender: TObject);
begin
  // ir para o endereço do ACBrSAC
  ShellExecute(Handle, 'open', PWideChar(lblUrlACBrSac1.Caption), '', '', 1);
end;

// quando clicar em alguma das urls chamar o link mostrado no caption
procedure TfrmPrincipal.URLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PWideChar(TLabel(Sender).Caption), '', '', 1);
end;

procedure TfrmPrincipal.wizPgInicioNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  // Verificar se o delphi está aberto
  {$IFNDEF DEBUG}
  if oACBr.AnyInstanceRunning then
  begin
    Stop := True;
    Application.MessageBox(
      'Feche a IDE do delphi antes de continuar.',
      PWideChar(Application.Title),
      MB_ICONERROR + MB_OK
    );
  end;
  {$ENDIF}
end;

procedure TfrmPrincipal.wizPgInstalacaoEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
var
  iFor: Integer;
begin
  // para 64 bit somente compilar
  if tPlatform = bpWin32 then // Win32
    btnInstalarACBr.Caption := 'Instalar'
  else // win64
    btnInstalarACBr.Caption := 'Compilar';

  lbInfo.Clear;
  for iFor := 0 to clbDelphiVersion.Count -1 do
  begin
     // Só pega os dados da 1a versão selecionada, para mostrar na tela qual vai iniciar
     if clbDelphiVersion.Checked[iFor] then
     begin
        lbInfo.Items.Add('Instalar : ' + clbDelphiVersion.Items[ifor] + ' ' + sPlatform);
        Break
     end;
  end;
end;

procedure TfrmPrincipal.wizPgInstalacaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  if (lstMsgInstalacao.Count <= 0) then
  begin
    Stop := True;
    Application.MessageBox(
      'Clique no botão instalar antes de continuar.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

//  Não deve ser permitido clicar em Next caso tenha havido erros.
//  Assim o código abaixo é desnecessário.
//  if (FCountErros > 0) then
//  begin
//    Stop := True;
//    Application.MessageBox(
//      'Ocorreram erros durante a compilação e instalação dos pacotes, verifique.',
//      'Erro.',
//      MB_OK + MB_ICONERROR
//    );
//  end;
end;

procedure TfrmPrincipal.wizPgConfiguracaoNextButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  // verificar se foi informado o diretório
  if Trim(edtDirDestino.Text) = EmptyStr then
  begin
    Stop := True;
    edtDirDestino.SetFocus;
    Application.MessageBox(
      'Diretório de instalação não foi informado.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

  if not ckbRemoveXMLSec.Checked then
  begin
    if MessageDlg('Usar XMLSec não é recomendado. Sugerimos que marque a opção "'+
                  ckbRemoveXMLSec.Caption + '" antes de continuar.'+ sLineBreak +
                  'Deseja continuar assim mesmo?', mtConfirmation, mbYesNo, 0, mbNo) <> mrYes  then
    begin
      Stop := True;
    end;
  end;

//  //Exibir mensagem abaixo apenas se não for o Delphi 7..
//  if not ckbRemoverCastWarnings.Checked then
//  begin
//    if MessageDlg('Se não estiver resolvendo os Warnings com strings sugerimos marcar a opção "'+
//                  ckbRemoverCastWarnings.Caption + '" antes de continuar.'+ sLineBreak +
//                  'Deseja continuar assim mesmo?', mtConfirmation, mbYesNo, 0, mbNo) <> mrYes  then
//    begin
//      Stop := True;
//    end;
//  end;
//
  if not ckbCopiarTodasDll.Checked then
  begin
    if MessageDlg('Não foi marcado a opção para copiar as DLLs. Você terá que copiar manualmente. ' + sLineBreak +
                  'Deseja continuar assim mesmo?', mtConfirmation, mbYesNo, 0, mbNo) <> mrYes  then
    begin
      Stop := True;
    end;
  end;

  // Gravar as configurações em um .ini para utilizar depois
  GravarConfiguracoesEmArquivoIni;

  ValidarSeExistemPacotesNasPastas(Stop, IncludeTrailingPathDelimiter(edtDirDestino.Text), framePacotes1.Pacotes);
end;

procedure TfrmPrincipal.btnVisualizarLogCompilacaoClick(Sender: TObject);
begin
  AbrirArquivoLogAtual;
end;

procedure TfrmPrincipal.wizPgSelectIDEsNextButtonClick(Sender: TObject; var Stop: Boolean);
var
  iFor: Integer;
  bChk: Boolean;
begin
  bChk := False;
  for iFor := 0 to clbDelphiVersion.Count -1 do
  begin
    if clbDelphiVersion.Checked[iFor] then
    begin
      bChk := True;
      Break;
    end;
  end;

  if not bChk then
  begin
    Stop := True;
    clbDelphiVersion.SetFocus;
    Application.MessageBox(
      'Para continuar escolha a versão do Delphi para a qual deseja instalar o ACBr.',
      'Erro.',
      MB_OK + MB_ICONERROR
    );
  end;

end;

procedure TfrmPrincipal.wizPrincipalCancelButtonClick(Sender: TObject);
begin
  if Application.MessageBox(
    'Deseja realmente cancelar a instalação?',
    'Fechar',
    MB_ICONQUESTION + MB_YESNO
  ) = ID_YES then
  begin
    Self.Close;
  end;
end;

procedure TfrmPrincipal.wizPrincipalFinishButtonClick(Sender: TObject);
begin
  Self.Close;
end;

end.
