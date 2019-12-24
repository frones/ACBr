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
  JclIDEUtils, JclCompilerUtils, ACBrPacotes;

type
  TDestino = (tdSystem, tdDelphi, tdNone);

  TOnIniciaNovaInstalacao = reference to procedure (const MaximoPassosProgresso: Integer;
        const NomeCaminhoArquivoLog: string; const Cabecalho: string);
  TOnInformarSituacao = reference to procedure (const Mensagem: string);
  TOnProgresso  = TProc;

  TACBrInstallOpcoes = record
    LimparArquivosACBrAntigos: Boolean;
    DeixarSomentePastasLib: Boolean;
    DeveInstalarCapicom: Boolean;
    DeveInstalarOpenSSL: Boolean;
    DeveCopiarOutrasDLLs: Boolean;
    DeveInstalarXMLSec: Boolean;
    UsarCargaTardiaDLL: Boolean;
    RemoverStringCastWarnings: Boolean;
    UsarCpp: Boolean;
    UsarUsarArquivoConfig: Boolean;
    sDestinoDLLs: TDestino;
    DiretorioRaizACBr: string;
  end;

  TACBrInstallComponentes = class(TObject)
  private
    FApp: TApplication;
    FOnIniciaNovaInstalacao: TOnIniciaNovaInstalacao;
    FOnProgresso: TOnProgresso;
    FOnInformaSituacao: TOnInformarSituacao;

    procedure FindDirs(InstalacaoAtual: TJclBorRADToolInstallation; APlatform:
        TJclBDSPlatform; ADirRoot: String; bAdicionar: Boolean = True);
    procedure CopiarArquivoDLLTo(ADestino : TDestino; const ANomeArquivo: String; const APathBin: string);

    procedure InstalarCapicom(ADestino : TDestino; const APathBin: string);
    procedure InstalarDiversos(ADestino: TDestino; const APathBin: string);
    procedure InstalarLibXml2(ADestino: TDestino; const APathBin: string);
    procedure InstalarOpenSSL(ADestino: TDestino; const APathBin: string);
    procedure InstalarXMLSec(ADestino: TDestino; const APathBin: string);

    procedure FazLog(const Texto: string; const ReiniciaArquivo: Boolean = False);
    procedure InformaSituacao(const Mensagem: string);
    procedure InformaProgresso;

    function RetornaPath(const ADestino: TDestino; const APathBin: string): string;
    procedure RemoverPacotesAntigos;
    procedure RemoverDiretoriosACBrDoPath;
    procedure RemoverArquivosAntigosDoDisco;

    procedure AdicionaLibraryPathNaDelphiVersaoEspecifica(const AProcurarRemover: string);
    procedure AddLibrarySearchPath;
    procedure DeixarSomenteLib;

    procedure CopiarOutrosArquivosParaPastaLibrary;
    procedure BeforeExecute(Sender: TJclBorlandCommandLineTool);
    procedure OutputCallLine(const Text: string);
    procedure CompilarEInstalarPacotes(ListaPacotes: TPacotes);
    procedure CompilarPacotes(const PastaACBr: string; listaPacotes: TPacotes);
    procedure InstalarPacotes(const PastaACBr: string; listaPacotes: TPacotes);
    function PathArquivoLog(const NomeVersao: string): String;

    procedure DesligarDefines;
    procedure FazInstalacaoInicial(ListaPacotes: TPacotes; UmaInstalacaoAtual: TJclBorRADToolInstallation);
    procedure InstalarOutrosRequisitos;
    procedure FazInstalacaoDLLs(const APathBin: string);

  public
    Opcoes: TACBrInstallOpcoes;

    InstalacaoAtual: TJclBorRADToolInstallation;
    tPlatformAtual: TJclBDSPlatform;
    sPlatform: string;
    sDirLibrary: string;

    FPacoteAtual: TFileName;

    ArquivoLog: string;

    FCountErros: Integer;
    JaCopiouDLLs: Boolean;

    constructor Create(app: TApplication);

    function Instalar(oACBr: TJclBorRADToolInstallations; ListaPacotes: TPacotes; ListaVersoesInstalacao:
        TList<Integer>): Boolean;

    property OnIniciaNovaInstalacao: TOnIniciaNovaInstalacao read FOnIniciaNovaInstalacao write FOnIniciaNovaInstalacao;
    property OnProgresso: TOnProgresso read FOnProgresso write FonProgresso;
    property OnInformaSituacao: TOnInformarSituacao read FOnInformaSituacao write FOnInformaSituacao;
  end;

implementation

uses
  ShellApi, Types, IOUtils,
  ACBrUtil, ACBrInstallUtils;


procedure TACBrInstallComponentes.OutputCallLine(const Text: string);
begin
  // Evento disparado a cada ação do compilador...

  // remover a warnings de conversão de string (delphi 2010 em diante)
  // as diretivas -W e -H não removem estas mensagens
  if (pos('Warning: W1057', Text) <= 0) and ((pos('Warning: W1058', Text) <= 0)) then
  begin
    FazLog(Text);
  end;
end;

procedure TACBrInstallComponentes.BeforeExecute(Sender: TJclBorlandCommandLineTool);
var
  LArquivoCfg: TFilename;
begin
  // Evento para setar os parâmetros do compilador antes de compilar

  // limpar os parâmetros do compilador
  Sender.Options.Clear;

  // não utilizar o dcc32.cfg
  if (InstalacaoAtual.SupportsNoConfig) and
     // -- Arquivo cfg agora opcional no caso de paths muito extensos
     (not Opcoes.UsarUsarArquivoConfig) then
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
  if Opcoes.UsarCpp then
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

  // -- Path para instalar os pacotes do Rave no D7, nas demais versões
  // -- o path existe.
  if InstalacaoAtual.VersionNumberStr = 'd7' then
    Sender.AddPathOption('U', InstalacaoAtual.RootDir + '\Rave5\Lib');

  // -- Na versão XE2 por motivo da nova tecnologia FireMonkey, deve-se adicionar
  // -- os prefixos dos nomes, para identificar se será compilado para VCL ou FMX
  if InstalacaoAtual.VersionNumberStr = 'd16' then
    Sender.Options.Add('-NSData.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi;System.Win');

  if MatchText(InstalacaoAtual.VersionNumberStr, ['d17','d18','d19','d20','d21','d22','d23','d24','d25','d26']) then
    Sender.Options.Add('-NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell');

  if (Opcoes.UsarUsarArquivoConfig) then
  begin
    LArquivoCfg := ChangeFileExt(FPacoteAtual, '.cfg');
    Sender.Options.SaveToFile(LArquivoCfg);
    Sender.Options.Clear;
  end;
end;

{ TACBrInstallComponentes }
constructor TACBrInstallComponentes.Create(app: TApplication);
begin
  inherited Create;
  //Valores padrões das opções
  Opcoes.LimparArquivosACBrAntigos := False;
  Opcoes.DeixarSomentePastasLib    := True;
  Opcoes.DeveInstalarCapicom       := False;
  Opcoes.DeveInstalarOpenSSL       := True;
  Opcoes.DeveCopiarOutrasDLLs      := True;
  Opcoes.DeveInstalarXMLSec        := False;
  Opcoes.UsarCargaTardiaDLL        := False;
  Opcoes.RemoverStringCastWarnings := False;
  Opcoes.UsarCpp                   := False;
  Opcoes.UsarUsarArquivoConfig     := True;
  Opcoes.sDestinoDLLs              := tdNone;
  Opcoes.DiretorioRaizACBr         := 'C:\ACBr\';

  ArquivoLog := '';

  FApp := app;
end;


procedure TACBrInstallComponentes.DeixarSomenteLib;
begin
  // remover os path com o segundo parametro
  FindDirs(InstalacaoAtual, tPlatformAtual, Opcoes.DiretorioRaizACBr + 'Fontes', False);
end;

procedure TACBrInstallComponentes.FazInstalacaoInicial(ListaPacotes: TPacotes; UmaInstalacaoAtual:
    TJclBorRADToolInstallation);
var
  Cabecalho: string;
  NomeVersao: string;
begin
  InstalacaoAtual := UmaInstalacaoAtual;
  tPlatformAtual  := bpWin32;
  sPlatform       := 'Win32';
  sDirLibrary     := Opcoes.DiretorioRaizACBr + 'Lib\Delphi\Lib' + AnsiUpperCase(InstalacaoAtual.VersionNumberStr);
  NomeVersao      := VersionNumberToNome(InstalacaoAtual.VersionNumberStr);

  ArquivoLog := PathArquivoLog(NomeVersao);
  Cabecalho := 'Versão do delphi: ' + NomeVersao + ' ' + sPlatform + sLineBreak +
               'Dir. Instalação : ' + Opcoes.DiretorioRaizACBr + sLineBreak +
               'Dir. Bibliotecas: ' + sDirLibrary;

  FazLog(Cabecalho + sLineBreak, True);

  if Assigned(OnIniciaNovaInstalacao) then
    FOnIniciaNovaInstalacao((ListaPacotes.Count * 2) + 6, ArquivoLog, Cabecalho);

  FCountErros := 0;

  // -- Evento disparado antes de iniciar a execução do processo.
  InstalacaoAtual.DCC32.OnBeforeExecute := BeforeExecute;
  // -- Evento para saidas de mensagens.
  InstalacaoAtual.OutputCallback := OutputCallLine;


  if Opcoes.LimparArquivosACBrAntigos then
  begin
    InformaSituacao('Removendo arquivos ACBr antigos dos discos...');
    RemoverArquivosAntigosDoDisco;
    InformaSituacao('...OK');
    InformaProgresso;
  end;

  InformaSituacao('Removendo instalação anterior do ACBr na IDE...');
  RemoverDiretoriosACBrDoPath;
  RemoverPacotesAntigos;


  InformaSituacao('...OK');
  InformaProgresso;

  // *************************************************************************
  // Cria diretório de biblioteca da versão do delphi selecionada,
  // só será criado se não existir
  // *************************************************************************
  InformaSituacao('Criando diretórios de bibliotecas...');
  ForceDirectories(sDirLibrary);
  InformaSituacao('...OK');
  InformaProgresso;

  // *************************************************************************
  // Adiciona os paths dos fontes na versão do delphi selecionada
  // *************************************************************************
  InformaSituacao('Adicionando library paths...');
  AddLibrarySearchPath;
  InformaSituacao('...OK');
  InformaProgresso;

  CompilarEInstalarPacotes(ListaPacotes);
end;

// retornar o caminho completo para o arquivo de logs
function TACBrInstallComponentes.PathArquivoLog(const NomeVersao: string): String;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) +
            'log_' + StringReplace(NomeVersao, ' ', '_', [rfReplaceAll]) + '.txt';
end;

procedure TACBrInstallComponentes.FazLog(const Texto: string; const ReiniciaArquivo: Boolean = False);
begin
  if ArquivoLog <> EmptyStr then
    WriteToTXT(ArquivoLog, Texto, not ReiniciaArquivo);
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
    if Opcoes.DeveInstalarCapicom then
    begin
      InstalarCapicom(Opcoes.sDestinoDLLs, APathBin);
      InformaSituacao('CAPICOM instalado com sucesso em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin));
    end;
  except
    on E: Exception do
    begin
      Inc(FCountErros);
      InformaSituacao('Ocorreu erro ao instalar a CAPICOM em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin) + sLineBreak +
            'Erro: ' + E.Message);
    end;
  end;
  // *************************************************************************
  // instalar OpenSSL
  // *************************************************************************
  try
    if Opcoes.DeveInstalarOpenSSL then
    begin
      InstalarOpenSSL(Opcoes.sDestinoDLLs, APathBin);
      InformaSituacao('OPENSSL instalado com sucesso em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin));
    end;
  except
    on E: Exception do
    begin
      Inc(FCountErros);
      InformaSituacao('Ocorreu erro ao instalar a OPENSSL em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin) + sLineBreak +
            'Erro: ' + E.Message);
    end;
  end;
  // *************************************************************************
  //instalar todas as "OUTRAS" DLLs
  // *************************************************************************
  if Opcoes.DeveCopiarOutrasDLLs then
  begin
    try
      InstalarLibXml2(Opcoes.sDestinoDLLs, APathBin);
      InformaSituacao('LibXml2 instalado com sucesso em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin));

      InstalarDiversos(Opcoes.sDestinoDLLs, APathBin);
      InformaSituacao('DLLs diversas instalado com sucesso em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin));

      if Opcoes.DeveInstalarXMLSec then
      begin
        InstalarXMLSec(Opcoes.sDestinoDLLs, APathBin);
        InformaSituacao('XMLSec instalado com sucesso em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin));
      end;
    except
      on E: Exception do
      begin
        Inc(FCountErros);
        InformaSituacao('Ocorreu erro ao instalar Outras DLL´s em '+ RetornaPath(Opcoes.sDestinoDLLs, APathBin) + sLineBreak +
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

procedure TACBrInstallComponentes.FindDirs(InstalacaoAtual: TJclBorRADToolInstallation;
    APlatform: TJclBDSPlatform; ADirRoot: String; bAdicionar: Boolean = True);

  function ExisteArquivoPasNoDir(const ADir: string): Boolean;
  var
    oDirList: TSearchRec;
  begin
    Result := False;
    if FindFirst(ADir + '*.pas', faNormal, oDirList) = 0 then
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
            InstalacaoAtual.RemoveFromLibrarySearchPath(ADirRoot + oDirList.Name, APlatform);
          end
          else
          begin
            if (not EProibido(oDirList.Name)) then
            begin
              if ExisteArquivoPasNoDir(oDirList.Name) then
              begin
                InstalacaoAtual.AddToLibrarySearchPath(ADirRoot + oDirList.Name, APlatform);
                InstalacaoAtual.AddToLibraryBrowsingPath(ADirRoot + oDirList.Name, APlatform);
              end;
              //-- Procura subpastas
              FindDirs(InstalacaoAtual, APlatform, ADirRoot + oDirList.Name, bAdicionar);
            end;
          end;
        end;
      until FindNext(oDirList) <> 0;
    finally
      SysUtils.FindClose(oDirList)
    end;
  end;
end;

procedure TACBrInstallComponentes.AddLibrarySearchPath;
var
  InstalacaoAtualCpp: TJclBDSInstallation;
begin
// adicionar o paths ao library path do delphi

  FindDirs(InstalacaoAtual, tPlatformAtual, Opcoes.DiretorioRaizACBr + 'Fontes');

  InstalacaoAtual.AddToLibraryBrowsingPath(sDirLibrary, tPlatformAtual);
  InstalacaoAtual.AddToLibrarySearchPath(sDirLibrary, tPlatformAtual);
  InstalacaoAtual.AddToDebugDCUPath(sDirLibrary, tPlatformAtual);

  // -- adicionar a library path ao path do windows

  AdicionaLibraryPathNaDelphiVersaoEspecifica('acbr');

  //-- ************ C++ Builder *************** //
  if Opcoes.UsarCpp then
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
end;

procedure TACBrInstallComponentes.RemoverDiretoriosACBrDoPath();
var
  ListaPaths: TStringList;
  I: Integer;
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
end;

procedure TACBrInstallComponentes.RemoverPacotesAntigos;
var
  I: Integer;
begin
  // remover pacotes antigos
  for I := InstalacaoAtual.IdePackages.Count - 1 downto 0 do
  begin
    if Pos('ACBR', AnsiUpperCase(InstalacaoAtual.IdePackages.PackageFileNames[I])) > 0 then
      InstalacaoAtual.IdePackages.RemovePackage(InstalacaoAtual.IdePackages.PackageFileNames[I]);
  end;
end;

procedure TACBrInstallComponentes.CopiarArquivoDLLTo(ADestino : TDestino; const ANomeArquivo: String;
     const APathBin: string);
var
  PathOrigem: String;
  PathDestino: String;
  DirSystem: String;

begin
  case ADestino of
    tdSystem: DirSystem := Trim(PathSystem);
    tdDelphi: DirSystem := APathBin;
  end;

  if DirSystem <> EmptyStr then
    DirSystem := IncludeTrailingPathDelimiter(DirSystem)
  else
    raise EFileNotFoundException.Create('Diretório de sistema não encontrado.');

  PathOrigem  := Opcoes.DiretorioRaizACBr + 'DLLs\' + ANomeArquivo;
  PathDestino := DirSystem + ExtractFileName(ANomeArquivo);

  if FileExists(PathOrigem) and not(FileExists(PathDestino)) then
  begin
    if not CopyFile(PWideChar(PathOrigem), PWideChar(PathDestino), True) then
    begin
      raise EFilerError.CreateFmt(
        'Ocorreu o seguinte erro ao tentar copiar o arquivo "%s": %d - %s', [
        ANomeArquivo, GetLastError, SysErrorMessage(GetLastError)
      ]);
    end;
  end;
end;

procedure TACBrInstallComponentes.InformaSituacao(const Mensagem: string);
begin
  FazLog(Mensagem);

  if Assigned(OnInformaSituacao) then
    OnInformaSituacao(Mensagem);
end;

function TACBrInstallComponentes.Instalar(oACBr: TJclBorRADToolInstallations; ListaPacotes: TPacotes;
      ListaVersoesInstalacao: TList<Integer>): Boolean;
var
  iListaVer: Integer;
begin
  DesligarDefines;
  JaCopiouDLLs := False;

  for iListaVer := 0 to oACBr.Count - 1 do
  begin
    // só instala as versão marcadas para instalar.
    if (not ListaVersoesInstalacao.Contains(iListaVer)) then
    begin
      Continue
    end;

    FazInstalacaoInicial(ListaPacotes, oACBr.Installations[iListaVer]);

    if (FCountErros <> 0) then
    begin
      Break;
    end;

    InstalarOutrosRequisitos;
  end;

  Result := (FCountErros = 0);
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
    CopiarArquivoDLLTo(ADestino,'Diversos\iconv.dll',    APathBin);
    CopiarArquivoDLLTo(ADestino,'Diversos\inpout32.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'Diversos\msvcr71.dll',  APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarLibXml2(ADestino: TDestino; const APathBin: string);
begin
  if ADestino <> tdNone then
  begin
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libxslt.dll',  APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libexslt.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libiconv.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'LibXml2\x86\libxml2.dll',  APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarOpenSSL(ADestino: TDestino; const APathBin: string);
begin
// copia as dlls da pasta openssl, estas dlls são utilizadas para assinar
// arquivos e outras coisas mais
  if ADestino <> tdNone then
  begin
    CopiarArquivoDLLTo(ADestino,'OpenSSL\1.0.2.20\x86\libeay32.dll', APathBin);
    CopiarArquivoDLLTo(ADestino,'OpenSSL\1.0.2.20\x86\ssleay32.dll', APathBin);
  end;
end;

procedure TACBrInstallComponentes.InstalarOutrosRequisitos;
begin
  InformaSituacao(sLineBreak+'INSTALANDO OUTROS REQUISITOS...');
  // *************************************************************************
  // deixar somente a pasta lib se for configurado assim
  // *************************************************************************
  if Opcoes.DeixarSomentePastasLib then
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
        InformaSituacao('Ocorreu erro ao copiar arquivos para: '+ sDirLibrary + sLineBreak +
              'Erro:'+ E.Message);
      end;
    end;
  end;


  if (FCountErros = 0) then
  begin
    if (Opcoes.sDestinoDLLs = tdDelphi) or (not JaCopiouDLLs) then
    begin
      FazInstalacaoDLLs(
            IncludeTrailingPathDelimiter(InstalacaoAtual.BinFolderName));
      JaCopiouDLLs := True;
    end;
  end;


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

procedure TACBrInstallComponentes.AdicionaLibraryPathNaDelphiVersaoEspecifica(const AProcurarRemover: string);
var
  PathsAtuais: string;
  ListaPaths: TStringList;
  I: Integer;
  wParam: Integer;
  lParam: Integer;
  lpdwResult: PDWORD_PTR;
  Resultado: Integer;
const
  cs: PChar = 'Environment Variables';
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
    // verificar se existe algo do ACBr e remover do environment variable PATH do delphi
    if Trim(AProcurarRemover) <> '' then
    begin
      for I := ListaPaths.Count - 1 downto 0 do
      begin
        if Pos(AnsiUpperCase(AProcurarRemover), AnsiUpperCase(ListaPaths[I])) > 0 then
          ListaPaths.Delete(I);
      end;
    end;
    // adicionar o path
    ListaPaths.Add(sDirLibrary);
    // escrever a variavel no override da ide
    InstalacaoAtual.ConfigData.WriteString(cs, 'PATH', ListaPaths.DelimitedText);
    // enviar um broadcast de atualização para o windows
    wParam := 0;
    lParam := LongInt(cs);
    lpdwResult := NIL;
    Resultado := SendMessageTimeout(HWND_BROADCAST, WM_SETTINGCHANGE, wParam, lParam, SMTO_NORMAL, 4000, lpdwResult);
    if Resultado = 0 then
      raise Exception.create('Ocorreu um erro ao tentar configurar o path: ' + SysErrorMessage(GetLastError));
  finally
    ListaPaths.Free;
  end;
end;

procedure TACBrInstallComponentes.CopiarOutrosArquivosParaPastaLibrary;
  procedure CopiarArquivosParaPastaLibrary(const Mascara : string);
  var
    ListArquivos: TStringDynArray;
    Arquivo : string;
    i: integer;
  begin
    ListArquivos := TDirectory.GetFiles(Opcoes.DiretorioRaizACBr + 'Fontes', Mascara, TSearchOption.soAllDirectories ) ;
    for i := Low(ListArquivos) to High(ListArquivos) do
    begin
      Arquivo := ExtractFileName(ListArquivos[i]);
      CopyFile(PWideChar(ListArquivos[i]), PWideChar(IncludeTrailingPathDelimiter(sDirLibrary) + Arquivo), False);
    end;
  end;
begin
  CopiarArquivosParaPastaLibrary('*.dcr');
  CopiarArquivosParaPastaLibrary('*.res');
  CopiarArquivosParaPastaLibrary('*.dfm');
  CopiarArquivosParaPastaLibrary('*.ini');
  CopiarArquivosParaPastaLibrary('*.inc');
end;

procedure TACBrInstallComponentes.DesligarDefines;
var
  ArquivoACBrInc: TFileName;
begin
  ArquivoACBrInc := Opcoes.DiretorioRaizACBr + 'Fontes\ACBrComum\ACBr.inc';
  DesligarDefineACBrInc(ArquivoACBrInc, 'DFE_SEM_OPENSSL', not Opcoes.DeveInstalarOpenSSL);
  DesligarDefineACBrInc(ArquivoACBrInc, 'DFE_SEM_CAPICOM', not Opcoes.DeveInstalarCapicom);
  DesligarDefineACBrInc(ArquivoACBrInc, 'USE_DELAYED', Opcoes.UsarCargaTardiaDLL);
  DesligarDefineACBrInc(ArquivoACBrInc, 'REMOVE_CAST_WARN', Opcoes.RemoverStringCastWarnings);
  DesligarDefineACBrInc(ArquivoACBrInc, 'DFE_SEM_XMLSEC', not Opcoes.DeveInstalarXMLSec);

end;
procedure TACBrInstallComponentes.RemoverArquivosAntigosDoDisco;
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
      ConteudoArquivo := ConteudoArquivo + 'del ACBr*.bpl ACBr*.dcp ACBr*.dcu PCN*.bpl PCN*.dcp PCN*.dcu SYNA*.bpl SYNA*.dcp SYNA*.dcu pnfs*.dcu pcte*.bpl pcte*.dcp pcte*.dcu pmdfe*.bpl pmdfe*.dcp pmdfe*.dcu pgnre*.dcp pgnre*.dcu pces*.dcp pces*.dcu pca*.dcp pca*.dcu /s' + sLineBreak;
      ConteudoArquivo := ConteudoArquivo + sLineBreak;
    end;

    WriteToTXT(PathBat, ConteudoArquivo, False);
  finally
    DriverList.Free;
  end;

  RunAsAdminAndWaitForCompletion(FApp.Handle, PathBat, FApp);
end;

procedure TACBrInstallComponentes.CompilarEInstalarPacotes(ListaPacotes: TPacotes);
begin
  // *************************************************************************
  // compilar os pacotes primeiramente
  // *************************************************************************
  InformaSituacao(sLineBreak+'COMPILANDO OS PACOTES...');
  CompilarPacotes(Opcoes.DiretorioRaizACBr, ListaPacotes);

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
    InstalarPacotes(Opcoes.DiretorioRaizACBr, ListaPacotes);
  end
  else
  begin
    InformaSituacao('Para a plataforma de 64 bits os pacotes são somente compilados.');
  end;

end;


procedure TACBrInstallComponentes.CompilarPacotes(const PastaACBr: string; listaPacotes: TPacotes);
var
  iDpk: Integer;
  NomePacote: string;
  sDirPackage: string;
begin
  for iDpk := 0 to listaPacotes.Count - 1 do
  begin
    if (not listaPacotes[iDpk].Checked) then
    begin
      InformaProgresso;
      Continue;
    end;

    NomePacote := listaPacotes[iDpk].Caption;
    // Busca diretório do pacote
    sDirPackage := FindDirPackage(IncludeTrailingPathDelimiter(PastaACBr) + 'Pacotes\Delphi', NomePacote);
    if (IsDelphiPackage(NomePacote)) then
    begin
      FazLog('');
      FPacoteAtual := sDirPackage + NomePacote;
      if InstalacaoAtual.CompilePackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
        InformaSituacao(Format('Pacote "%s" compilado com sucesso.', [NomePacote]))
      else
      begin
        Inc(FCountErros);
        InformaSituacao(Format('Erro ao compilar o pacote "%s".', [NomePacote]));
        // parar no primeiro erro para evitar de compilar outros pacotes que
        // precisam do pacote que deu erro
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
          if InstalacaoAtual.UninstallPackage(sDirPackage + NomePacote, sDirLibrary, sDirLibrary) then
            InformaSituacao(Format('Pacote "%s" removido com sucesso...', [NomePacote]));
        end;
      end;
    end;
    InformaProgresso;
  end;
end;




end.
