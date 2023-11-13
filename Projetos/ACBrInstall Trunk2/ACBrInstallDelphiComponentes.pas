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

unit ACBrInstallDelphiComponentes;

interface

uses
  SysUtils, Windows, Messages, Classes, Forms, System.Generics.Collections,
  JclIDEUtils, JclCompilerUtils, ACBrPacotes, UACBrPlataformaInstalacaoAlvo;

const
  cVersaoConfig = '1.0';

type
  TDestino = (tdSystem, tdDelphi, tdNone);
  TNivelLog = (nlNenhumLog, nlMinimo, nlMedio, nlMaximo);

  TOnIniciaNovaInstalacao = reference to procedure (const MaximoPassosProgresso: Integer;
        const NomeCaminhoArquivoLog: string; const Cabecalho: string);
  TOnInformarSituacao = reference to procedure (const Mensagem: string);
  TOnProgresso  = TProc;

  TACBrCompilerOpcoes = record
    DeveInstalarCapicom: Boolean;
    DeveInstalarOpenSSL: Boolean;
    DeveInstalarXMLSec: Boolean;
    UsarCargaTardiaDLL: Boolean;
    RemoverStringCastWarnings: Boolean;
    DeveSobrescreverDllsExistentes: Boolean;
  public
    procedure DesligarDefines(const ArquivoACBrInc: TFileName);
    procedure RedefinirValoresOpcoesParaPadrao;
    procedure CarregarDeArquivoIni(const ArquivoIni: string);
    procedure SalvarEmArquivoIni(const ArquivoIni: string);
  end;

  TACBrInstallOpcoes = record
    LimparArquivosACBrAntigos: Boolean;
    DeixarSomentePastasLib: Boolean;
    UsarCpp: Boolean;
    UsarUsarArquivoConfig: Boolean;
    sDestinoDLLs: TDestino;
    DiretorioRaizACBr: string;
    DeveCopiarOutrasDLLs: Boolean;
  public
    procedure RedefinirValoresOpcoesParaPadrao;
    procedure CarregarDeArquivoIni(const ArquivoIni: string);
    procedure SalvarEmArquivoIni(const ArquivoIni: string);
  end;

  TACBrInstallComponentes = class(TObject)
  private
    FApp: TApplication;
    FOnIniciaNovaInstalacao: TOnIniciaNovaInstalacao;
    FOnProgresso: TOnProgresso;
    FOnInformaSituacao: TOnInformarSituacao;

    FUmaPlataformaDestino: TACBrPlataformaInstalacaoAlvo;

    FPacoteAtual: TFileName;

    FArquivoLog: string;
    FNivelLog: TNivelLog;

    FCountErros: Integer;
    FJaCopiouDLLs: Boolean;
    FJaFezLimpezaArquivoACBrAntigos: Boolean;

    procedure FindDirs(APlatform:TJclBDSPlatform; ADirRoot: String; bAdicionar: Boolean = True);
    procedure CopiarArquivoDLLTo(ADestino : TDestino; const ANomeArquivo: String; const APathBin: string);

    procedure InstalarCapicom(ADestino : TDestino; const APathBin: string);
    procedure InstalarDiversos(ADestino: TDestino; const APathBin: string);
    procedure InstalarLibXml2(ADestino: TDestino; const APathBin: string);
    procedure InstalarOpenSSL(ADestino: TDestino; const APathBin: string);
    procedure InstalarXMLSec(ADestino: TDestino; const APathBin: string);

    procedure FazLog(const Texto: string; const ANivelLog: TNivelLog = nlMedio; const ReiniciaArquivo: Boolean = False);
    procedure InformaSituacao(const Mensagem: string);
    procedure InformaProgresso;

    function RetornaPath(const ADestino: TDestino; const APathBin: string): string;
    procedure RemoverPacotesAntigos;
    procedure RemoverDiretoriosACBrDoPath;
    procedure RemoverArquivosAntigosDoDisco;

    procedure AdicionaEnvironmentPathNaVersaoEspecificaDoDelphi(const AProcurarRemover: string);
    procedure AddLibrarySearchPath;
    procedure DeixarSomenteLib;

    procedure ApagarOutrosArquivosDaPastaLibrary(const PastarLibrary: string);
    procedure CopiarOutrosArquivosParaPastaLibrary;
    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure CompilaPacotePorNomeArquivo(const NomePacote: string);
    procedure OutputCallLine(const Text: string);
    procedure CompilarEInstalarPacotes(ListaPacotes: TPacotes);
    procedure CompilarPacotes(const PastaACBr: string; listaPacotes: TPacotes);
    procedure InstalarPacotes(const PastaACBr: string; listaPacotes: TPacotes);
    function PathArquivoLog(const NomeVersao: string): String;

    procedure FazInstalacaoInicial(ListaPacotes: TPacotes; UmaPlataformaDestino: TACBrPlataformaInstalacaoAlvo);
    procedure InstalarOutrosRequisitos;
    procedure FazInstalacaoDLLs(const APathBin: string);
    procedure ConfiguraMetodosCompiladores;
    function FazBroadcastDeAlteracaoDeConfiguracao(cs: PWideChar) : Integer;

  public
    OpcoesInstall: TACBrInstallOpcoes;
    OpcoesCompilacao: TACBrCompilerOpcoes;

    constructor Create(app: TApplication);
    destructor Destroy; override;

    procedure LerDeArquivoIni(const ArquivoIni: string);
    procedure SalvarConfiguracoesEmArquivoIni(const ArquivoIni: string);

    function Instalar(ListaPacotes: TPacotes; ListaVersoesInstalacao:TList<Integer>;
      ListaPlataformasInstalacao: TListaPlataformasAlvos): Boolean;

    property OnIniciaNovaInstalacao: TOnIniciaNovaInstalacao read FOnIniciaNovaInstalacao write FOnIniciaNovaInstalacao;
    property OnProgresso: TOnProgresso read FOnProgresso write FonProgresso;
    property OnInformaSituacao: TOnInformarSituacao read FOnInformaSituacao write FOnInformaSituacao;
  end;

  function sVersaoInstalador: string;

implementation

uses
  ShellApi, Types, IOUtils,
  ACBrUtil.FilesIO, ACBrUtil.Strings, ACBrInstallUtils, IniFiles,
  JvVersionInfo;


function sVersaoInstalador: string;
var
  jvVerInfo: TJvVersionInfo;
begin
  jvVerInfo := AppVerInfo;
  try
  Result := jvVerInfo.FileVersion;
  finally
    jvVerInfo.Free;
  end;
end;

{ TACBrInstallComponentes }
constructor TACBrInstallComponentes.Create(app: TApplication);
begin
  inherited Create;
  OpcoesInstall.RedefinirValoresOpcoesParaPadrao;
  OpcoesCompilacao.RedefinirValoresOpcoesParaPadrao;

  FArquivoLog := '';
  FNivelLog  := nlMedio;
  FJaFezLimpezaArquivoACBrAntigos := False;

  FApp := app;
//  UmaPlataformaDestino := TPlataformaDestino.Create;
//  oACBr := TJclBorRADToolInstallations.Create;
//  tcpt  := TCompileTargetList.Create;

end;

destructor TACBrInstallComponentes.Destroy;
begin
//  tcpt.Free;
//  oACBr.Free;
//  UmaPlataformaDestino.Free;
  inherited;
end;

procedure TACBrInstallComponentes.ConfiguraMetodosCompiladores;
begin
  // -- Evento disparado antes de iniciar a execução do processo.
  FUmaPlataformaDestino.InstalacaoAtual.DCC32.OnBeforeExecute := BeforeExecute;
  if clDcc64 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCC64.OnBeforeExecute := BeforeExecute;
  if clDccOSX32 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCOSX32.OnBeforeExecute := BeforeExecute;
  if clDccOSX64 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCOSX64.OnBeforeExecute := BeforeExecute;
  if clDcciOSSimulator in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCiOSSimulator.OnBeforeExecute := BeforeExecute;
  if clDcciOS32 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCiOS32.OnBeforeExecute := BeforeExecute;
  if clDcciOS64 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCiOS64.OnBeforeExecute := BeforeExecute;
  if clDccArm32 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCArm32.OnBeforeExecute := BeforeExecute;
  if clDccArm64 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCArm64.OnBeforeExecute := BeforeExecute;
  if clDccLinux64 in FUmaPlataformaDestino.InstalacaoAtual.CommandLineTools then
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).DCCLinux64.OnBeforeExecute := BeforeExecute;

  // -- Evento para saidas de mensagens.
  FUmaPlataformaDestino.InstalacaoAtual.OutputCallback := OutputCallLine;
end;

procedure TACBrInstallComponentes.OutputCallLine(const Text: string);
begin
  // Evento disparado a cada ação do compilador...

  // remover a warnings de conversão de string (delphi 2010 em diante)
  // as diretivas -W e -H não removem estas mensagens
  if (pos('Warning: W1057', Text) <= 0) and ((pos('Warning: W1058', Text) <= 0)) then
  begin
    FazLog(Text);
  end;

  if (Pos('This version of the product does not support command line compiling', Text) > 0) then
  begin
    //Encontramos um Delphi trial. Precisamos abortar a instalação.
    Inc(FCountErros);
    FazLog('O ACBrInstall precisa usar o compilador por linha de comando. Na sua versão o compilador por linha de comando está desativado.');
    FazLog('Geralmente isso acontece com versões Trial ou Community Edition do Delphi.');
    FazLog('Você precisará instalar os pacotes manualmente.');
  end;
end;

procedure TACBrInstallComponentes.BeforeExecute(Sender: TJclBorlandCommandLineTool);
const
  VersoesComNamespaces: array[0..13] of string = ('d16', 'd17','d18','d19','d20','d21','d22','d23','d24','d25',
                                                  'd26','d27', 'd28','d29');
  NamespacesBase = 'System;Xml;Data;Datasnap;Web;Soap;';
  NamespacesWindows = 'Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Winapi;System.Win;';
  NamespacesOSX = 'Macapi;Posix;System.Mac;';
  NamespacesAndroid = '';
  NamespacesiOS = '';
  NamespacesVCL = 'Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;';
  NamespacesFMX = 'FMX;FMX.ASE;FMX.Bind;FMX.Canvas;FMX.DAE;FMX.DateTimeControls;FMX.EmbeddedControls;FMX.Filter;FMX.ListView;FMX.MediaLibrary;';
var
  LArquivoCfg: TFilename;
  NamespacesTemp: string;

begin
  with FUmaPlataformaDestino do
  begin
    // Evento para setar os parâmetros do compilador antes de compilar

    // limpar os parâmetros do compilador
    Sender.Options.Clear;

    // não utilizar o dcc32.cfg
    if (InstalacaoAtual.SupportsNoConfig) and
       // -- Arquivo cfg agora opcional no caso de paths muito extensos
       (not OpcoesInstall.UsarUsarArquivoConfig) then
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
    Sender.AddPathOption('U', InstalacaoAtual.LibFolderName[tPlatformAtual]);
    Sender.AddPathOption('U', InstalacaoAtual.LibrarySearchPath[tPlatformAtual]);
    Sender.AddPathOption('U', sDirLibrary);
    // -I<paths> = Include directories
    Sender.AddPathOption('I', InstalacaoAtual.LibrarySearchPath[tPlatformAtual]);
    // -R<paths> = Resource directories
    Sender.AddPathOption('R', InstalacaoAtual.LibrarySearchPath[tPlatformAtual]);
    // -N0<path> = unit .dcu output directory
    Sender.AddPathOption('N0', sDirLibrary);
    Sender.AddPathOption('LE', sDirLibrary);
    Sender.AddPathOption('LN', sDirLibrary);

    // ************ C++ Builder *************** //
    if OpcoesInstall.UsarCpp then
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

    //Montar namespaces:
    if MatchText(InstalacaoAtual.VersionNumberStr, VersoesComNamespaces) then
    begin
  //    Namespaces := '';

      NamespacesTemp := NamespacesBase;

      if tPlatformAtual in [bpWin32, bpWin64] then
      begin
        NamespacesTemp := NamespacesTemp + NamespacesWindows;

        if tPlatformAtual = bpWin32 then
        begin
          NamespacesTemp := NamespacesTemp + 'Bde;';
        end;
      end;

      if tPlatformAtual in [bpWin32, bpWin64] {and notFMX} then
      begin
        NamespacesTemp := NamespacesTemp + NamespacesVCL;
      end;

      if not (tPlatformAtual in [bpWin32, bpWin64]) {or FMX} then
      begin
        NamespacesTemp := NamespacesTemp + NamespacesFMX;
      end;

      if tPlatformAtual = bpOSX32 then
      begin
        NamespacesTemp := NamespacesTemp + NamespacesOSX;
      end;

      Sender.Options.Add('-NS'+NamespacesTemp);

    end;

    if (OpcoesInstall.UsarUsarArquivoConfig) then
    begin
      LArquivoCfg := ChangeFileExt(FPacoteAtual, '.cfg');
      Sender.Options.SaveToFile(LArquivoCfg);
      Sender.Options.Clear;
    end;
  end;//<---End With Temporário
end;

procedure TACBrInstallComponentes.DeixarSomenteLib;
begin
  // remover os path com o segundo parametro
  FindDirs(FUmaPlataformaDestino.tPlatformAtual, OpcoesInstall.DiretorioRaizACBr + 'Fontes', False);
end;

procedure TACBrInstallComponentes.FazInstalacaoInicial(ListaPacotes: TPacotes; UmaPlataformaDestino:
   TACBrPlataformaInstalacaoAlvo);
var
  Cabecalho: string;
  NomeVersao: string;
begin

  with UmaPlataformaDestino do
  begin
    NomeVersao := VersionNumberToNome(InstalacaoAtual.VersionNumberStr);

    FArquivoLog := PathArquivoLog(NomeVersao+ ' ' + sPlatform);
    Cabecalho := 'Versao Instalador: ' + sVersaoInstalador + sLineBreak +
                 'Versão do delphi: ' + NomeVersao + ' ' + sPlatform + sLineBreak +
                 'Dir. Instalação : ' + OpcoesInstall.DiretorioRaizACBr + sLineBreak +
                 'Dir. Bibliotecas: ' + sDirLibrary;

    FazLog(Cabecalho + sLineBreak, nlMinimo, True);

    if Assigned(OnIniciaNovaInstalacao) then
      FOnIniciaNovaInstalacao((ListaPacotes.Count * 2) + 6, FArquivoLog, Cabecalho);

    FCountErros := 0;

    // limpar arquivos antigos somente ao iniciar o procedimento de instalação
    if (OpcoesInstall.LimparArquivosACBrAntigos) and (not FJaFezLimpezaArquivoACBrAntigos)  then
    begin
      FJaFezLimpezaArquivoACBrAntigos := True;
      InformaSituacao('Removendo arquivos ACBr antigos dos discos...');
      RemoverArquivosAntigosDoDisco;
      InformaSituacao('...OK');
    end;
    //se a opção não estiver marcada deve informar o progresso também...
    InformaProgresso;

    ConfiguraMetodosCompiladores;

    InformaSituacao('Removendo librarypaths da instalação anterior do ACBr na IDE...');
    RemoverDiretoriosACBrDoPath;
    InformaSituacao('...OK');

    if tPlatformAtual = bpWin32 then
    begin
      InformaSituacao('Removendo pacotes 32bits da instalação anterior do ACBr na IDE...');
      RemoverPacotesAntigos;
      InformaSituacao('...OK');
    end;
    InformaProgresso;

    // *************************************************************************
    // Cria diretório de biblioteca da versão do delphi selecionada,
    // só será criado se não existir
    // *************************************************************************
    InformaSituacao('Criando diretórios de bibliotecas para ' + sPlatform + '...');
    ForceDirectories(sDirLibrary);
    ApagarOutrosArquivosDaPastaLibrary(sDirLibrary);
    InformaSituacao('...OK');
    InformaProgresso;

    // *************************************************************************
    // Adiciona os paths dos fontes na versão do delphi selecionada
    // *************************************************************************
    InformaSituacao('Adicionando library paths para ' + sPlatform + '...');
    AddLibrarySearchPath;
    InformaSituacao('...OK');
    InformaProgresso;

    // -- adicionar ao environment variables do delphi
    if tPlatformAtual = bpWin32 then
    begin
      InformaSituacao('Alterando a variável de ambiente PATH do Delphi...');
      AdicionaEnvironmentPathNaVersaoEspecificaDoDelphi('acbr');
      InformaSituacao('...OK');
    end;
    InformaProgresso;


    CompilarEInstalarPacotes(ListaPacotes);
  end; //<---- endwith
end;

// retornar o caminho completo para o arquivo de logs
function TACBrInstallComponentes.PathArquivoLog(const NomeVersao: string): String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
            'log_' + StringReplace(NomeVersao, ' ', '_', [rfReplaceAll]) + '.txt';
end;

procedure TACBrInstallComponentes.FazLog(const Texto: string; const ANivelLog: TNivelLog = nlMedio; const
    ReiniciaArquivo: Boolean = False);
begin
  if ANivelLog > FNivelLog then
  begin
    Exit
  end;

  if FArquivoLog <> EmptyStr then
    WriteToTXT(FArquivoLog, AnsiString(Texto), not ReiniciaArquivo);
end;

function TACBrInstallComponentes.RetornaPath(const ADestino: TDestino; const APathBin: string): string;
begin
  case ADestino of
    tdSystem: Result := '"'+ PathSystem + '"';
    tdDelphi: Result := '"'+ APathBin + '"';
    tdNone:   Result := 'Tipo de destino "nenhum" não aceito!';
  else
    Result := 'Tipo de destino desconhecido!'
  end;
end;

procedure TACBrInstallComponentes.FazInstalacaoDLLs(const APathBin: string);
begin
  // *************************************************************************
  // instalar capicom
  // *************************************************************************
  try
    if OpcoesCompilacao.DeveInstalarCapicom then
    begin
      InstalarCapicom(OpcoesInstall.sDestinoDLLs, APathBin);
      InformaSituacao('CAPICOM instalado com sucesso em '+ RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin));
    end;
  except
    on E: Exception do
    begin
      Inc(FCountErros);
      InformaSituacao('Ocorreu erro ao instalar a CAPICOM em '+ RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin) + sLineBreak +
            'Erro: ' + E.Message);
    end;
  end;

  // *************************************************************************
  // instalar OpenSSL
  // *************************************************************************
  try
    if OpcoesCompilacao.DeveInstalarOpenSSL then
    begin
      InstalarOpenSSL(OpcoesInstall.sDestinoDLLs, APathBin);
      InformaSituacao('OPENSSL instalado com sucesso em '+ RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin));
    end;
  except
    on E: Exception do
    begin
      Inc(FCountErros);
      InformaSituacao('Ocorreu erro ao instalar a OPENSSL em '+ RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin) + sLineBreak +
            'Erro: ' + E.Message);
    end;
  end;

  // *************************************************************************
  //instalar todas as "OUTRAS" DLLs
  // *************************************************************************
  if OpcoesInstall.DeveCopiarOutrasDLLs then
  begin
    try
      InstalarLibXml2(OpcoesInstall.sDestinoDLLs, APathBin);
      InformaSituacao('LibXml2 instalado com sucesso em '+ RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin));

      InstalarDiversos(OpcoesInstall.sDestinoDLLs, APathBin);
      InformaSituacao('DLLs diversas instalado com sucesso em '+ RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin));

      if OpcoesCompilacao.DeveInstalarXMLSec then
      begin
        InstalarXMLSec(OpcoesInstall.sDestinoDLLs, APathBin);
        InformaSituacao('XMLSec instalado com sucesso em '+ RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin));
      end;
    except
      on E: Exception do
      begin
        Inc(FCountErros);
        InformaSituacao(
          'Ocorreu erro ao instalar Outras DLL´s em '+
          RetornaPath(OpcoesInstall.sDestinoDLLs, APathBin) + sLineBreak +
          'Erro: ' + E.Message);
      end;
    end;
  end;
end;

procedure TACBrInstallComponentes.InformaProgresso;
begin
  if Assigned(FOnProgresso) then
    FOnProgresso;
end;

procedure TACBrInstallComponentes.FindDirs(APlatform: TJclBDSPlatform; ADirRoot: String; bAdicionar: Boolean = True);

  function ExisteArquivoPasNoDir(const ADir: string): Boolean;
  var
    oDirList: TSearchRec;
  begin
    Result := False;
    if FindFirst(IncludeTrailingPathDelimiter(ADir) + '*.pas', faNormal, oDirList) = 0 then
    begin
      try
        Result := True;
      finally
        SysUtils.FindClose(oDirList)
      end;
    end;
  end;

  function EProibido(const ADir: String): Boolean;
  const
    LISTA_PROIBIDOS: ARRAY[0..14] OF STRING = (
      'quick', 'rave', 'laz', 'VerificarNecessidade', '__history', '__recovery', 'backup',
      'Logos', 'Colorido', 'PretoBranco', 'Imagens', 'bmp', 'logotipos', 'PerformanceTest', 'test'
    );
  var
    Str: String;
  begin
//    Result := False;
    for str in LISTA_PROIBIDOS do
    begin
      Result := Pos(AnsiUpperCase(str), AnsiUpperCase(ADir)) > 0;
      if Result then
        Break;
    end;
  end;

var
  oDirList: TSearchRec;
  Conseguiu: Boolean;
begin
  with FUmaPlataformaDestino do
  begin
    ADirRoot := IncludeTrailingPathDelimiter(ADirRoot);

    if FindFirst(ADirRoot + '*.*', faDirectory, oDirList) = 0 then
    begin
      try
        repeat
          if ((oDirList.Attr and faDirectory) <> 0) and
              (oDirList.Name <> '.')                and
              (oDirList.Name <> '..') then
          begin
            if not bAdicionar then
            begin
              Conseguiu := InstalacaoAtual.RemoveFromLibrarySearchPath(ADirRoot + oDirList.Name, APlatform);
              FazLog('Conseguiu remover do Library Search Path: '+ ADirRoot + oDirList.Name + '....' +
                      BoolToStr(Conseguiu, True), nlMaximo);
              //-- Procura subpastas
              FindDirs(APlatform, ADirRoot + oDirList.Name, bAdicionar);
            end
            else
            begin
              if (not EProibido(oDirList.Name)) then
              begin
                if ExisteArquivoPasNoDir(ADirRoot + oDirList.Name) then
                begin
                  InstalacaoAtual.AddToLibrarySearchPath(ADirRoot + oDirList.Name, APlatform);
                  InstalacaoAtual.AddToLibraryBrowsingPath(ADirRoot + oDirList.Name, APlatform);
                end;
                //-- Procura subpastas
                FindDirs(APlatform, ADirRoot + oDirList.Name, bAdicionar);
              end;
            end;
          end;
        until FindNext(oDirList) <> 0;
      finally
        SysUtils.FindClose(oDirList)
      end;
    end;
  end; //---endwith
end;

procedure TACBrInstallComponentes.AddLibrarySearchPath;
var
  InstalacaoAtualCpp: TJclBDSInstallation;
begin
  // adicionar o paths ao library path do delphi
  with FUmaPlataformaDestino do
  begin
    InstalacaoAtual.AddToLibraryBrowsingPath(sDirLibrary, tPlatformAtual);
    InstalacaoAtual.AddToLibrarySearchPath(sDirLibrary, tPlatformAtual);
    InstalacaoAtual.AddToDebugDCUPath(sDirLibrary, tPlatformAtual);

    FindDirs(tPlatformAtual, OpcoesInstall.DiretorioRaizACBr + 'Fontes');

    //-- ************ C++ Builder *************** //
    if OpcoesInstall.UsarCpp then
    begin
       if InstalacaoAtual is TJclBDSInstallation then
       begin
         InstalacaoAtualCpp := TJclBDSInstallation(InstalacaoAtual);
         InstalacaoAtualCpp.AddToCppSearchPath(sDirLibrary, tPlatformAtual);
         InstalacaoAtualCpp.AddToCppLibraryPath(sDirLibrary, tPlatformAtual);
         InstalacaoAtualCpp.AddToCppBrowsingPath(sDirLibrary, tPlatformAtual);
         InstalacaoAtualCpp.AddToCppIncludePath(sDirLibrary, tPlatformAtual);
       end;
    end;
  end;//---endwith
end;

procedure TACBrInstallComponentes.RemoverDiretoriosACBrDoPath();
var
  ListaPaths: TStringList;
  I: Integer;
begin
  with FUmaPlataformaDestino do
  begin
    ListaPaths := TStringList.Create;
    try
      ListaPaths.StrictDelimiter := True;
      ListaPaths.Delimiter := ';';

      // remover do search path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := InstalacaoAtual.RawLibrarySearchPath[tPlatformAtual];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
      InstalacaoAtual.RawLibrarySearchPath[tPlatformAtual] := ListaPaths.DelimitedText;
      // remover do browse path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := InstalacaoAtual.RawLibraryBrowsingPath[tPlatformAtual];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
      InstalacaoAtual.RawLibraryBrowsingPath[tPlatformAtual] := ListaPaths.DelimitedText;
      // remover do Debug DCU path
      ListaPaths.Clear;
      ListaPaths.DelimitedText := InstalacaoAtual.RawDebugDCUPath[tPlatformAtual];
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos('ACBR', AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
      InstalacaoAtual.RawDebugDCUPath[tPlatformAtual] := ListaPaths.DelimitedText;
    finally
      ListaPaths.Free;
    end;
  end;//---endwith
end;

procedure TACBrInstallComponentes.RemoverPacotesAntigos;
var
  I: Integer;
begin
  with FUmaPlataformaDestino do
  begin
    // remover pacotes antigos
    for I := InstalacaoAtual.IdePackages.Count - 1 downto 0 do
    begin
      if Pos('ACBR', AnsiUpperCase(InstalacaoAtual.IdePackages.PackageFileNames[I])) > 0 then
        InstalacaoAtual.IdePackages.RemovePackage(InstalacaoAtual.IdePackages.PackageFileNames[I]);
    end;
  end;//---endwith
end;

procedure TACBrInstallComponentes.CopiarArquivoDLLTo(ADestino : TDestino; const ANomeArquivo: String;
     const APathBin: string);
var
  PathOrigem: String;
  PathDestino: String;
  DirSystem: String;
  VaiSobrescrever: Boolean;
begin
  VaiSobrescrever := OpcoesCompilacao.DeveSobrescreverDllsExistentes;

  case ADestino of
    tdSystem: DirSystem := Trim(PathSystem);
    tdDelphi: DirSystem := APathBin;
  end;

  if DirSystem <> EmptyStr then
    DirSystem := IncludeTrailingPathDelimiter(DirSystem)
  else
    raise EFileNotFoundException.Create('Diretório de sistema não encontrado.');

  PathOrigem  := OpcoesInstall.DiretorioRaizACBr + 'DLLs\' + ANomeArquivo;
  PathDestino := DirSystem + ExtractFileName(ANomeArquivo);

  if (FileExists(PathDestino)) and (not VaiSobrescrever) then
  begin
    InformaSituacao(Format('AVISO: Arquivo já se encontra no destino. Não sobrescrito: "%s"', [PathDestino]));
    Exit;
  end;

  if not FileExists(PathOrigem) then
  begin
    InformaSituacao(Format('ERRO: Arquivo não encontrado na origem: "%s"', [PathOrigem]));
    raise EFileNotFoundException.Create(Format('ERRO: Arquivo não encontrado na origem: "%s"', [PathOrigem]));
  end;

  if not CopyFile(PWideChar(PathOrigem), PWideChar(PathDestino), (not VaiSobrescrever)) then
  begin
    raise EFilerError.CreateFmt(
      'Ocorreu o seguinte erro ao tentar copiar o arquivo "%s": %d - %s', [
      ANomeArquivo, GetLastError, SysErrorMessage(GetLastError)
    ]);
  end;
end;

procedure TACBrInstallComponentes.InformaSituacao(const Mensagem: string);
begin
  FazLog(Mensagem);

  if Assigned(OnInformaSituacao) then
    OnInformaSituacao(Mensagem);
end;

function TACBrInstallComponentes.Instalar(ListaPacotes: TPacotes; ListaVersoesInstalacao:TList<Integer>;
    ListaPlataformasInstalacao: TListaPlataformasAlvos): Boolean;
var
  I: Integer;
begin
  OpcoesCompilacao.DesligarDefines(OpcoesInstall.DiretorioRaizACBr + 'Fontes\ACBrComum\ACBr.inc');
  FJaCopiouDLLs := False;
  FJaFezLimpezaArquivoACBrAntigos := False;

  for I := 0 to ListaVersoesInstalacao.Count -1 do
  begin
    FUmaPlataformaDestino := ListaPlataformasInstalacao[ListaVersoesInstalacao[i]];
    FUmaPlataformaDestino.sDirLibrary := OpcoesInstall.DiretorioRaizACBr + FUmaPlataformaDestino.GetDirLibrary;

    FazInstalacaoInicial(ListaPacotes, FUmaPlataformaDestino);

    if (FCountErros <> 0) then
      Break;

    InstalarOutrosRequisitos;
  end;

  Result := (FCountErros = 0);
end;

function TACBrInstallComponentes.FazBroadcastDeAlteracaoDeConfiguracao(cs: PWideChar) : Integer;
var
  wParam: Integer;
  lParam: Integer;
  lpdwResult: PDWORD_PTR;
begin
  // enviar um broadcast de atualização para o windows
  wParam := 0;
  lParam := LongInt(cs);
  lpdwResult := nil;
  Result := SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, wParam, lParam, SMTO_NORMAL, 4000, lpdwResult);
end;

procedure TACBrInstallComponentes.InstalarCapicom(ADestino : TDestino; const APathBin: string);
begin
// copia as dlls da pasta capcom para a pasta escolhida pelo usuario e registra a dll
  if ADestino <> tdNone then
  begin
    CopiarArquivoDLLTo(ADestino, 'Capicom\capicom.dll', APathBin);
    CopiarArquivoDLLTo(ADestino, 'Capicom\msxml5.dll',  APathBin);
    CopiarArquivoDLLTo(ADestino, 'Capicom\msxml5r.dll', APathBin);

    if ADestino = tdDelphi then
    begin
      RegistrarActiveXServer(APathBin + 'capicom.dll', True);
      RegistrarActiveXServer(APathBin + 'msxml5.dll', True);
    end
    else
    begin
      RegistrarActiveXServer('capicom.dll', True);
      RegistrarActiveXServer('msxml5.dll', True);
    end;
  end;
end;

//copia as dlls da pasta Diversoso para a pasta escolhida pelo usuario
procedure TACBrInstallComponentes.InstalarDiversos(ADestino: TDestino; const APathBin: string);
begin
  if ADestino <> tdNone then
  begin
    CopiarArquivoDLLTo(ADestino,'Diversos\x86\iconv.dll',    APathBin);
    CopiarArquivoDLLTo(ADestino,'Diversos\x86\inpout32.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'Diversos\x86\msvcr71.dll',  APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarLibXml2(ADestino: TDestino; const APathBin: string);
begin
  if ADestino <> tdNone then
  begin
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libexslt.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libiconv.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libxml2.dll',  APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libxslt.dll',  APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarOpenSSL(ADestino: TDestino; const APathBin: string);
begin
// copia as dlls da pasta openssl, estas dlls são utilizadas para assinar
// arquivos e outras coisas mais
  if ADestino <> tdNone then
  begin
    CopiarArquivoDLLTo(ADestino,'OpenSSL\1.1.1.10\x86\libcrypto-1_1.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'OpenSSL\1.1.1.10\x86\libssl-1_1.dll', APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarOutrosRequisitos;
begin
  with FUmaPlataformaDestino do
  begin
    InformaSituacao(sLineBreak+'INSTALANDO OUTROS REQUISITOS...');
    // *************************************************************************
    // deixar somente a pasta lib se for configurado assim
    // *************************************************************************
    if OpcoesInstall.DeixarSomentePastasLib and (tPlatformAtual in [bpWin32{, bpWin64}]) then
    begin
      try
        DeixarSomenteLib;
        InformaSituacao('Limpeza library path com sucesso');
      except
        on E: Exception do
        begin
          InformaSituacao('Ocorreu erro ao limpar o path: ' + sLineBreak + E.Message);
        end;
      end;

      try
        CopiarOutrosArquivosParaPastaLibrary;
        InformaSituacao('Cópia dos arquivos necessário feita com sucesso para: '+ sDirLibrary);
      except
        on E: Exception do
        begin
          InformaSituacao(
            'Ocorreu erro ao copiar arquivos para: '+ sDirLibrary + sLineBreak +
            'Erro:'+ E.Message);
        end;
      end;
    end;


    if (FCountErros = 0) then
    begin
      // Copiar apenas dlls na plataforma da IDE Win32.
      if (tPlatformAtual = bpWin32) and
         ((OpcoesInstall.sDestinoDLLs = tdDelphi) or (not FJaCopiouDLLs)) then
      begin
        FazInstalacaoDLLs(IncludeTrailingPathDelimiter(InstalacaoAtual.BinFolderName));
        FJaCopiouDLLs := True;
      end;
    end;
  end;//---endwith
end;

procedure TACBrInstallComponentes.InstalarXMLSec(ADestino: TDestino; const APathBin: string);
begin
  //copia as dlls da pasta XMLSec para a pasta escolhida pelo usuario
  if ADestino <> tdNone then
  begin
    CopiarArquivoDLLTo(ADestino, 'XMLSec\iconv.dll', APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxml2.dll', APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxmlsec.dll', APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxmlsec-openssl.dll', APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\libxslt.dll', APathBin);
    CopiarArquivoDLLTo(ADestino, 'XMLSec\zlib1.dll', APathBin);
  end;
end;

procedure TACBrInstallComponentes.LerDeArquivoIni(const ArquivoIni: string);
begin
  OpcoesInstall.CarregarDeArquivoIni(ArquivoIni);
  OpcoesCompilacao.CarregarDeArquivoIni(ArquivoIni);
end;

procedure TACBrInstallComponentes.SalvarConfiguracoesEmArquivoIni(const ArquivoIni: string);
begin
  OpcoesInstall.SalvarEmArquivoIni(ArquivoIni);
  OpcoesCompilacao.SalvarEmArquivoIni(ArquivoIni);
end;

procedure TACBrInstallComponentes.AdicionaEnvironmentPathNaVersaoEspecificaDoDelphi(const AProcurarRemover: string);
var
  PathsAtuais: string;
  ListaPaths: TStringList;
  I: Integer;
  Resultado: Integer;
const
  cs: PChar = 'Environment Variables';
begin
  with FUmaPlataformaDestino do
  begin
    // tentar ler o path configurado na ide do delphi, se não existir ler
    // a atual para complementar e fazer o override
    PathsAtuais := Trim(InstalacaoAtual.EnvironmentVariables.Values['PATH']);
    if PathsAtuais = '' then
      PathsAtuais := GetEnvironmentVariable('PATH');
    // manipular as strings
    ListaPaths := TStringList.Create;
    try
      ListaPaths.Clear;
      ListaPaths.Delimiter := ';';
      ListaPaths.StrictDelimiter := True;
      ListaPaths.DelimitedText := PathsAtuais;
      // verificar se existe algo do ACBr e remover apenas se for Win32
      if (Trim(AProcurarRemover) <> '') and (tPlatformAtual = bpWin32) then
      begin
        for I := ListaPaths.Count - 1 downto 0 do
        begin
          if Pos(AnsiUpperCase(AProcurarRemover), AnsiUpperCase(ListaPaths[I])) > 0 then
            ListaPaths.Delete(I);
        end;
      end;
      // adicionar ao path a pasta da biblioteca
      ListaPaths.Add(sDirLibrary);
      InstalacaoAtual.ConfigData.WriteString(cs, 'PATH', ListaPaths.DelimitedText);

      //Isso é realmente necessário??
      Resultado := FazBroadcastDeAlteracaoDeConfiguracao(cs);
      if Resultado = 0 then
        raise Exception.create('Ocorreu um erro ao tentar configurar o path: ' + SysErrorMessage(GetLastError));
    finally
      ListaPaths.Free;
    end;
  end;//---endwith
end;

procedure TACBrInstallComponentes.CompilaPacotePorNomeArquivo(const NomePacote: string);
begin
  if FUmaPlataformaDestino.InstalacaoAtual.RadToolKind = brBorlandDevStudio then
  begin
    (FUmaPlataformaDestino.InstalacaoAtual as TJclBDSInstallation).CleanPackageCache(BinaryFileName(FUmaPlataformaDestino.sDirLibrary, FPacoteAtual));
  end;
  if FUmaPlataformaDestino.InstalacaoAtual.CompilePackage(FPacoteAtual, FUmaPlataformaDestino.sDirLibrary, FUmaPlataformaDestino.sDirLibrary) then
  begin
    InformaSituacao(Format('Pacote "%s" compilado com sucesso.', [NomePacote]))
  end
  else
  begin
    Inc(FCountErros);
    InformaSituacao(Format('Erro ao compilar o pacote "%s".', [NomePacote]));
    Exit;
  end;
end;

procedure TACBrInstallComponentes.ApagarOutrosArquivosDaPastaLibrary(const PastarLibrary: string);
  procedure ApagarArquivosDaPastaLibrary(const Mascara : string);
  var
    ListArquivos: TStringDynArray;
    Arquivo : string;
    i: integer;
  begin
    ListArquivos := TDirectory.GetFiles(IncludeTrailingPathDelimiter(PastarLibrary), Mascara, TSearchOption.soAllDirectories);
    for i := Low(ListArquivos) to High(ListArquivos) do
    begin
      Arquivo := ExtractFileName(ListArquivos[i]);
      DeleteFile(PWideChar(ListArquivos[i]));
    end;
  end;
begin
  ApagarArquivosDaPastaLibrary('*.dcr');
  ApagarArquivosDaPastaLibrary('*.res');
  ApagarArquivosDaPastaLibrary('*.dfm');
  ApagarArquivosDaPastaLibrary('*.ini');
  ApagarArquivosDaPastaLibrary('*.inc');
end;

procedure TACBrInstallComponentes.CopiarOutrosArquivosParaPastaLibrary;
  procedure CopiarArquivosParaPastaLibrary(const Mascara : string);
  var
    ListArquivos: TStringDynArray;
    Arquivo : string;
    i: integer;
  begin
    with FUmaPlataformaDestino do
    begin
      ListArquivos := TDirectory.GetFiles(OpcoesInstall.DiretorioRaizACBr + 'Fontes', Mascara, TSearchOption.soAllDirectories ) ;
      for i := Low(ListArquivos) to High(ListArquivos) do
      begin
        Arquivo := ExtractFileName(ListArquivos[i]);
        CopyFile(PWideChar(ListArquivos[i]), PWideChar(IncludeTrailingPathDelimiter(sDirLibrary) + Arquivo), False);
      end;
    end;//----endwith
  end;
begin
  CopiarArquivosParaPastaLibrary('*.dcr');
  CopiarArquivosParaPastaLibrary('*.res');
  CopiarArquivosParaPastaLibrary('*.dfm');
  CopiarArquivosParaPastaLibrary('*.ini');
  CopiarArquivosParaPastaLibrary('*.inc');
end;

procedure TACBrInstallComponentes.RemoverArquivosAntigosDoDisco;
const
  SMascaraArquivoQueSeraoRemovidos = 'ACBr*.bpl ACBr*.dcp ACBr*.dcu DCLACBr*.bpl  DCLACBr*.dcp DCLACBr*.dcu '+
    'PCN*.bpl PCN*.dcp PCN*.dcu '+
    'pnfs*.dcu pcte*.bpl pcte*.dcp pcte*.dcu pmdfe*.bpl pmdfe*.dcp pmdfe*.dcu pgnre*.dcp '+
    'pgnre*.bpl pces*.bpl pgnre*.dcu pces*.dcp pces*.dcu pca*.dcp pca*.dcu';

var
  PathBat: String;
  DriverList: TStringList;
  ConteudoArquivo: String;
  I: Integer;
begin
  PathBat := ExtractFilePath(ParamStr(0)) + 'apagarACBr.bat';

  // listar driver para montar o ConteudoArquivo
  DriverList := TStringList.Create;
  try
    GetDriveLetters(DriverList);
    ConteudoArquivo := '@echo off' + sLineBreak;
    for I := 0 to DriverList.Count -1 do
    begin
      ConteudoArquivo := ConteudoArquivo + StringReplace(DriverList[I], '\', '', []) + sLineBreak;
      ConteudoArquivo := ConteudoArquivo + 'cd\' + sLineBreak;
      ConteudoArquivo := ConteudoArquivo + 'del '+ SMascaraArquivoQueSeraoRemovidos +' /s' + sLineBreak;
      ConteudoArquivo := ConteudoArquivo + sLineBreak;
    end;

    WriteToTXT(PathBat, AnsiString(ConteudoArquivo), False);
  finally
    DriverList.Free;
  end;

  RunAsAdminAndWaitForCompletion(FApp.Handle, PathBat, FApp);
end;

procedure TACBrInstallComponentes.CompilarEInstalarPacotes(ListaPacotes: TPacotes);
begin
  with FUmaPlataformaDestino do
  begin
    // *************************************************************************
    // compilar os pacotes primeiramente
    // *************************************************************************
    if not (tPlatformAtual in [bpWin32{, bpWin64}]) then
    begin
      InformaSituacao(sLineBreak+'No momento não estamos compilando os pacotes da plataforma ' + sPlatform +'.');
      Exit;
    end;

    InformaSituacao(sLineBreak+'COMPILANDO OS PACOTES...');
    CompilarPacotes(OpcoesInstall.DiretorioRaizACBr, ListaPacotes);

    // *************************************************************************
    // instalar os pacotes somente se não ocorreu erro na compilação e plataforma for Win32
    // *************************************************************************
    if FCountErros > 0 then
    begin
      InformaSituacao('Abortando... Ocorreram erros na compilação dos pacotes.');
      Exit;
    end;

    if ( tPlatformAtual = bpWin32) then
    begin
      InformaSituacao(sLineBreak+'INSTALANDO OS PACOTES...');
      InstalarPacotes(OpcoesInstall.DiretorioRaizACBr, ListaPacotes);
    end
    else
    begin
      InformaSituacao('Para a plataforma de 64 bits os pacotes são somente compilados.');
    end;

  end;//---endwith
end;


procedure TACBrInstallComponentes.CompilarPacotes(const PastaACBr: string; listaPacotes: TPacotes);
var
  iDpk: Integer;
  NomePacote: string;
  sDirPackage: string;
begin
  FUmaPlataformaDestino.ConfiguraDCCPelaPlataformaAtual;

  for iDpk := 0 to listaPacotes.Count - 1 do
  begin
    if (not listaPacotes[iDpk].MarcadoParaInstalar) then
    begin
      InformaProgresso;
      Continue;
    end;

    NomePacote := listaPacotes[iDpk].GetNome;
    if not (IsDelphiPackage(NomePacote)) then
    begin
      FazLog(Format('"%s" não é um pacote Delphi. Pulando pacote.', [NomePacote]));
      InformaProgresso;
      Continue;
    end;

    if not (listaPacotes[iDpk].SuportaVersao(FUmaPlataformaDestino.InstalacaoAtual.IDEPackageVersionNumber)) then
    begin
      FazLog(Format('Versão "%s" não suportada para o pacote "%s". Pulando pacote.',
                    [IntToStr(FUmaPlataformaDestino.InstalacaoAtual.VersionNumber), NomePacote]) );
      InformaProgresso;
      Continue;
    end;
    FazLog('');

    // Busca diretório completo do pacote
    sDirPackage := FindDirPackage(IncludeTrailingPathDelimiter(PastaACBr) + 'Pacotes\Delphi', NomePacote);
    FPacoteAtual := sDirPackage + NomePacote;

    CompilaPacotePorNomeArquivo(NomePacote);
    if FCountErros> 0 then
    begin
      // Parar no primeiro erro para evitar de compilar outros pacotes que
      // dependem desse que ocasionou erro.
      Break;
    end;

    //Compilar também o pacote Design Time se a plataforma form Win32
    if (FUmaPlataformaDestino.tPlatformAtual = bpWin32) and FileExists(sDirPackage + 'DCL'+ NomePacote) then
    begin
      FazLog('');
      FPacoteAtual := sDirPackage + 'DCL'+ NomePacote;
      CompilaPacotePorNomeArquivo('DCL'+ NomePacote);
      if FCountErros> 0 then
      begin
        // Parar no primeiro erro para evitar de compilar outros pacotes que
        // dependem desse que ocasionou erro.
        Break;
      end;
    end;
    InformaProgresso;
  end;
end;

procedure TACBrInstallComponentes.InstalarPacotes(const PastaACBr: string; listaPacotes: TPacotes);
var
  iDpk: Integer;
  NomePacote: string;
  bRunOnly: Boolean;
  sDirPackage: string;
begin
  for iDpk := 0 to listaPacotes.Count - 1 do
  begin
    NomePacote := listaPacotes[iDpk].GetNome;
    if not IsDelphiPackage(NomePacote) then
    begin
      InformaProgresso;
      Continue;
    end;

    if not (listaPacotes[iDpk].SuportaVersao(FUmaPlataformaDestino.InstalacaoAtual.IDEPackageVersionNumber)) then
    begin
      FazLog(Format('Versão "%s" não suportada para o pacote "%s". Pulando pacote.',
                    [IntToStr(FUmaPlataformaDestino.InstalacaoAtual.VersionNumber), NomePacote]) );
      InformaProgresso;
      Continue;
    end;

    // Busca diretório do pacote
    sDirPackage := FindDirPackage(IncludeTrailingPathDelimiter(PastaACBr) + 'Pacotes\Delphi', NomePacote);
    FPacoteAtual := sDirPackage + NomePacote;
    GetDPKFileInfo(FPacoteAtual, bRunOnly);

    if bRunOnly then
    begin
      //Encontrar o pacote DesignTime correspondente caso exista
      if FileExists(sDirPackage + 'DCL'+ NomePacote) then
      begin
        FPacoteAtual := sDirPackage + 'DCL'+ NomePacote;
        GetDPKFileInfo(FPacoteAtual, bRunOnly);
      end;
    end;

    // Se continuou Runonly, instalar somente os pacotes de designtime
    if bRunOnly then
    begin
      InformaProgresso;
      Continue;
    end;

    // se o pacote estiver marcado instalar, senão desinstalar
    if listaPacotes[iDpk].MarcadoParaInstalar then
    begin
      if FUmaPlataformaDestino.InstalacaoAtual.InstallPackage(FPacoteAtual, FUmaPlataformaDestino.sDirLibrary, FUmaPlataformaDestino.sDirLibrary) then
        InformaSituacao(Format('Pacote "%s" instalado com sucesso.', [NomePacote]))
      else
      begin
        Inc(FCountErros);
        InformaSituacao(Format('Ocorreu um erro ao instalar o pacote "%s".', [NomePacote]));
        Break;
      end;
    end
    else
    begin
      if FUmaPlataformaDestino.InstalacaoAtual.UninstallPackage(FPacoteAtual, FUmaPlataformaDestino.sDirLibrary, FUmaPlataformaDestino.sDirLibrary) then
        InformaSituacao(Format('Pacote "%s" removido com sucesso...', [NomePacote]));
    end;
    InformaProgresso;
  end;
end;


{ TACBrCompilerOpcoes }

procedure TACBrCompilerOpcoes.CarregarDeArquivoIni(const ArquivoIni: string);
var
  ArqIni: TIniFile;
begin
  RedefinirValoresOpcoesParaPadrao;
  ArqIni := TIniFile.Create(ArquivoIni);
  try

    DeveInstalarCapicom       := ArqIni.ReadBool('CONFIG','InstalaCapicom', DeveInstalarCapicom);
    DeveInstalarOpenSSL       := ArqIni.ReadBool('CONFIG','InstalaOpenSSL', DeveInstalarOpenSSL);
//    DeveInstalarXMLSec        := False;
    UsarCargaTardiaDLL        := ArqIni.ReadBool('CONFIG','CargaDllTardia', True);
    RemoverStringCastWarnings := ArqIni.ReadBool('CONFIG','RemoverCastWarnings', RemoverStringCastWarnings);
    DeveSobrescreverDllsExistentes := ArqIni.ReadBool('CONFIG','SobrescreverDLL', DeveSobrescreverDllsExistentes);;
  finally
    ArqIni.Free;
  end;

end;

procedure TACBrCompilerOpcoes.DesligarDefines(const ArquivoACBrInc: TFileName);
begin
  DesligarDefineACBrInc(ArquivoACBrInc, 'DFE_SEM_OPENSSL', not DeveInstalarOpenSSL);
  DesligarDefineACBrInc(ArquivoACBrInc, 'DFE_SEM_CAPICOM', not DeveInstalarCapicom);
  DesligarDefineACBrInc(ArquivoACBrInc, 'USE_DELAYED', UsarCargaTardiaDLL);
  DesligarDefineACBrInc(ArquivoACBrInc, 'REMOVE_CAST_WARN', RemoverStringCastWarnings);
  DesligarDefineACBrInc(ArquivoACBrInc, 'DFE_SEM_XMLSEC', not DeveInstalarXMLSec);
end;

procedure TACBrCompilerOpcoes.RedefinirValoresOpcoesParaPadrao;
begin
  DeveInstalarCapicom            := False;
  DeveInstalarOpenSSL            := True;
  DeveInstalarXMLSec             := False;
  UsarCargaTardiaDLL             := True;
  RemoverStringCastWarnings      := True;
  DeveSobrescreverDllsExistentes := False;
end;

procedure TACBrCompilerOpcoes.SalvarEmArquivoIni(const ArquivoIni: string);
var
  ArqIni: TIniFile;
begin
  ArqIni := TIniFile.Create(ArquivoIni);
  try
    ArqIni.WriteBool('CONFIG','InstalaOpenSSL', DeveInstalarOpenSSL);
    ArqIni.WriteBool('CONFIG','InstalaCapicom', DeveInstalarCapicom);
//    ArqIni.WriteBool('CONFIG','InstalaXmlSec', DeveInstalarXMLSec);
    ArqIni.WriteBool('CONFIG','CargaDllTardia', UsarCargaTardiaDLL);
    ArqIni.WriteBool('CONFIG','RemoverCastWarnings', RemoverStringCastWarnings);
    ArqIni.WriteBool('CONFIG','SobrescreverDLL', DeveSobrescreverDllsExistentes);
  finally
    ArqIni.Free;
  end;

end;

{ TACBrInstallOpcoes }

procedure TACBrInstallOpcoes.RedefinirValoresOpcoesParaPadrao;
begin
  LimparArquivosACBrAntigos := False;
  DeixarSomentePastasLib    := True;
  UsarCpp                   := False;
  UsarUsarArquivoConfig     := True;
  sDestinoDLLs              := tdSystem;
  DiretorioRaizACBr         := ExtractFilePath(ParamStr(0));
  DeveCopiarOutrasDLLs      := True;
end;

procedure TACBrInstallOpcoes.SalvarEmArquivoIni(const ArquivoIni: string);
var
  ArqIni: TIniFile;
begin
  ArqIni := TIniFile.Create(ArquivoIni);
  try
    ArqIni.WriteString('CONFIG','VersaoArquivoIniConfig', cVersaoConfig);

    ArqIni.WriteString('CONFIG', 'DiretorioInstalacao', DiretorioRaizACBr);
    ArqIni.WriteBool('CONFIG','DexarSomenteLib', DeixarSomentePastasLib);
    ArqIni.WriteBool('CONFIG','C++Builder', UsarCpp);
    case sDestinoDLLs of
      tdSystem:
      begin
        ArqIni.WriteInteger('CONFIG','DestinoDLL', 0);
      end;
      tdDelphi:
      begin
        ArqIni.WriteInteger('CONFIG','DestinoDLL', 1);
      end;
      tdNone:
      begin
        ArqIni.WriteInteger('CONFIG','DestinoDLL', 2);
      end;
    end;
  finally
    ArqIni.Free;
  end;

end;

procedure TACBrInstallOpcoes.CarregarDeArquivoIni(const ArquivoIni: string);
var
  ArqIni: TIniFile;
begin
  RedefinirValoresOpcoesParaPadrao;

  ArqIni := TIniFile.Create(ArquivoIni);
  try
//  LimparArquivosACBrAntigos := False;
    DeixarSomentePastasLib    := ArqIni.ReadBool('CONFIG','DexarSomenteLib', DeixarSomentePastasLib);
    UsarCpp                   := ArqIni.ReadBool('CONFIG','C++Builder', UsarCpp);
//    UsarUsarArquivoConfig     := True;
    case ArqIni.ReadInteger('CONFIG','DestinoDLL', 0) of
      0 : sDestinoDLLs := tdSystem;
      1 : sDestinoDLLs := tdDelphi;
      2 : sDestinoDLLs := tdNone;
    else
      sDestinoDLLs     := tdSystem;
    end;

    DiretorioRaizACBr := ArqIni.ReadString('CONFIG', 'DiretorioInstalacao', DiretorioRaizACBr);
//    DeveCopiarOutrasDLLs      := True;
  finally
    ArqIni.Free;
  end;
end;

end.
