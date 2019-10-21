{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2011 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Regys silveira, Isaque Pinheiro                 }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrAAC;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  ACBrBase, ACBrPAFClass, SysUtils, Classes;

type
  EACBrAAC                       = class(Exception);
  EACBrAAC_CRC                   = class(EACBrAAC);
  EACBrAAC_ArqNaoEncontrado      = class(EACBrAAC);
  EACBrAAC_SemNomeArquivo        = class(EACBrAAC);
  EACBrAAC_SemChave              = class(EACBrAAC);
  EACBrAAC_SemResposta           = class(EACBrAAC);
  EACBrAAC_ArquivoInvalido       = class(EACBrAAC);
  EACBrAAC_NumSerieNaoEncontrado = class(EACBrAAC);
  EACBrAAC_ValorGTInvalido       = class(EACBrAAC);

  TACBrAACOnCrypt   = procedure( ConteudoArquivo : AnsiString;
     var Resposta : AnsiString ) of object ;
  TACBrAACOnDeCrypt = procedure( ConteudoCriptografado : AnsiString;
     var Resposta : AnsiString ) of object ;
  TACBrAACGetChave = procedure(var Chave: AnsiString) of object ;
  TACBrAACAntesArquivo = procedure( var Continua: Boolean) of object ;
  TACBrAACOnVerificarRecomporValorGT = procedure(const NumSerie: String;
     var ValorGT : Double) of object ;
  TACBrAACOnVerificarRecomporNumSerie = procedure(const NumSerie: String;
     const ValorGT : Double; var CRO: Integer; var CNI: Integer) of object ;
  { TACBrAAC }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrAAC = class( TACBrComponent )
  private
     fsArqLOG : String ;
     fsCriarBAK : Boolean ;
     fsDtHrArquivo : TDateTime ;
     fsEfetuarFlush : Boolean ;
     fsGravarDadosPAF : Boolean ;
     fsGravarDadosSH : Boolean ;
     fsGravarTodosECFs : Boolean ;
     fsNomeArquivoAux : String ;
     fsNomeCompleto : String ;
     fsOnAntesAbrirArquivo : TACBrAACAntesArquivo ;
     fsOnAntesGravarArquivo : TACBrAACAntesArquivo ;
     fsOnCrypt : TACBrAACOnCrypt ;
     fsOnDeCrypt : TACBrAACOnDeCrypt ;
     fsOnDepoisAbrirArquivo : TNotifyEvent ;
     fsOnDepoisGravarArquivo : TNotifyEvent ;
     fsOnGetChave : TACBrAACGetChave ;
     fsOnVerificarRecomporNumSerie : TACBrAACOnVerificarRecomporNumSerie ;
     fsOnVerificarRecomporValorGT : TACBrAACOnVerificarRecomporValorGT ;
     fsParams : TStringList ;
     fsIdentPAF: TACBrECFIdentificacaoPAF;
     fsGravarConfigApp: Boolean;
     function GetChave : AnsiString ;
     procedure SetNomeArquivoAux(const AValue : String) ;
     procedure SetParams(const AValue : TStringList) ;
     function GetArquivoInvalido: Boolean;
  protected

     function Criptografar( const Dados: AnsiString ) : AnsiString ;
     function DesCriptografar( const Dados: AnsiString ) : AnsiString ;
     Procedure GravaLog( const AString: AnsiString );
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AchaECF( const NumSerie : String ) : TACBrAACECF ;
    function AchaIndiceECF(const NumeroSerie: String): Integer;

    property Chave : AnsiString read GetChave ;

    procedure AbrirArquivo;
    procedure SalvarArquivo;

    function VerificarGTECF(const NumeroSerie: String;
      var ValorGT: Double): Integer ;
    procedure AtualizarMD5(const AMD5: String);
    procedure AtualizarValorGT(const NumeroSerie: String;
      const ValorGT: Double);
    procedure VerificaReCarregarArquivo;

    property DtHrArquivo : TDateTime read fsDtHrArquivo ;
    property ArquivoInvalido: Boolean read GetArquivoInvalido;
  published
    property NomeArquivoAux : String  read fsNomeArquivoAux
       write SetNomeArquivoAux ;
    property CriarBAK : Boolean read fsCriarBAK write fsCriarBAK default True;
    property EfetuarFlush : Boolean read fsEfetuarFlush write fsEfetuarFlush default True;
    property GravarDadosSH  : Boolean read fsGravarDadosSH
       write fsGravarDadosSH default True;
    property GravarDadosPAF : Boolean read fsGravarDadosPAF
       write fsGravarDadosPAF default True;
    property GravarTodosECFs : Boolean read fsGravarTodosECFs
       write fsGravarTodosECFs default True;
    property GravarConfigApp  : Boolean read fsGravarConfigApp
       write fsGravarConfigApp default True;
    property ArqLOG : String  read fsArqLOG write fsArqLOG ;

    property IdentPAF    : TACBrECFIdentificacaoPAF
       read fsIdentPAF write fsIdentPAF;

    property Params : TStringList read fsParams write SetParams ;

    { Eventos }
    property OnCrypt   : TACBrAACOnCrypt   read fsOnCrypt    write fsOnCrypt ;
    property OnDeCrypt : TACBrAACOnDeCrypt read fsOnDeCrypt  write fsOnDeCrypt ;
    property OnGetChave: TACBrAACGetChave  read fsOnGetChave write fsOnGetChave ;
    property OnAntesAbrirArquivo : TACBrAACAntesArquivo read fsOnAntesAbrirArquivo
      write fsOnAntesAbrirArquivo ;
    property OnDepoisAbrirArquivo : TNotifyEvent read fsOnDepoisAbrirArquivo
      write fsOnDepoisAbrirArquivo ;
    property OnAntesGravarArquivo : TACBrAACAntesArquivo read fsOnAntesGravarArquivo
      write fsOnAntesGravarArquivo ;
    property OnDepoisGravarArquivo : TNotifyEvent
      read fsOnDepoisGravarArquivo write fsOnDepoisGravarArquivo ;
    property VerificarRecomporValorGT : TACBrAACOnVerificarRecomporValorGT
      read fsOnVerificarRecomporValorGT write fsOnVerificarRecomporValorGT;
    property VerificarRecomporNumSerie : TACBrAACOnVerificarRecomporNumSerie
      read fsOnVerificarRecomporNumSerie write fsOnVerificarRecomporNumSerie;
  end;

implementation

Uses
  IniFiles, math,
  ACBrUtil;

{ TACBrAAC }

constructor TACBrAAC.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fsParams          := TStringList.Create;
  fsIdentPAF        := TACBrECFIdentificacaoPAF.Create;

  fsOnCrypt    := nil ;
  fsOnDeCrypt  := nil ;
  fsOnGetChave := nil ;
  fsOnAntesAbrirArquivo   := nil ;
  fsOnDepoisAbrirArquivo  := nil ;
  fsOnAntesGravarArquivo  := nil ;
  fsOnDepoisGravarArquivo := nil ;
  fsOnVerificarRecomporNumSerie := nil;
  fsOnVerificarRecomporValorGT  := nil;

  fsNomeArquivoAux    := '' ;
  fsCriarBAK          := True;
  fsEfetuarFlush      := True;
  fsGravarDadosSH     := True;
  fsGravarDadosPAF    := True;
  fsGravarTodosECFs   := True;
  fsGravarConfigApp   := True;
  fsDtHrArquivo       := 0 ;
end ;

destructor TACBrAAC.Destroy ;
begin
  fsIdentPAF.Free;
  fsParams.Free;

  inherited Destroy ;
end ;

function TACBrAAC.AchaECF(const NumSerie : String) : TACBrAACECF ;
Var
  I : Integer ;
begin
  GravaLog( 'AchaECF( '+NumSerie+' )' );

  VerificaReCarregarArquivo;

  Result := nil;
  I      := 0 ;
  while (Result = nil) and (I < fsIdentPAF.ECFsAutorizados.Count) do
  begin
    if Trim( NumSerie ) = Trim( fsIdentPAF.ECFsAutorizados[I].NumeroSerie ) then
       Result := fsIdentPAF.ECFsAutorizados[I]
    else
       Inc( I ) ;
  end ;

  if not Assigned(Result) then
     GravaLog( '  Falha' )
  else
     GravaLog( '  Ok' );
end ;

procedure TACBrAAC.AbrirArquivo ;
var
  Ini: TMemIniFile;
  SL : TStringList;
  MS : TMemoryStream ;
  I  : Integer ;
  S, R : AnsiString ;
  Linha, Ident : String ;
  Continua : Boolean ;
  CRC : Word ;
begin
  GravaLog( 'AbrirArquivo');

  Continua := True;
  if Assigned( fsOnAntesAbrirArquivo ) then
     fsOnAntesAbrirArquivo( Continua );

  if not Continua then
  begin
     GravaLog( 'AbrirArquivo abortado' );
     exit;
  end ;

  if NomeArquivoAux = '' then
     raise EACBrAAC_SemNomeArquivo.Create( ACBrStr('Nome do Arquivo não Informado em: ACBrAAC.NomeArquivoAux') ) ;

  if not FileExists( fsNomeCompleto ) then
     raise EACBrAAC_ArqNaoEncontrado.Create(
        ACBrStr( 'Arquivo Auxiliar Criptografado'+sLineBreak+
                 '"'+NomeArquivoAux+'"'+sLineBreak+
                 'não encontrado') );

  {$IFDEF DELPHI2007_UP}
  FileAge( fsNomeCompleto, fsDtHrArquivo );
  {$ELSE}
  fsDtHrArquivo := FileDateToDateTime( FileAge( fsNomeCompleto ) );
  {$ENDIF}

  // Lê arquivo de modo binário e transfere para a AnsiString = S //
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile( fsNomeCompleto );
    MS.Position := 0;
    SetLength(S, MS.Size);
    MS.ReadBuffer(PAnsiChar(S)^, MS.Size);
  finally
    MS.Free;
  end;

  SL  := TStringList.Create ;
  Ini := TMemIniFile.Create('') ;
  try
     // DEBUG
     // GravaLog('Arquivo Lido: '+sLineBreak+ S );
     R := Trim(DesCriptografar( S )) ;
     // DEBUG
     //GravaLog('Arquivo Descriptografado: '+sLineBreak+ R );

     if (R <> '') then
     begin
       I := PosLast('CRC:', R);
       if (I > 0) then
       begin
         CRC := StrToIntDef( copy( R, I+4, 20 ), -999) ;

         if StringCrc16( copy(R, 1, I-1 ) ) <> CRC then
           fsDtHrArquivo := 0;
       end
     end;

     // Atribuindo para o .INI //
     SL.Text := R;
     Ini.SetStrings( SL );

     if GravarDadosSH then
     begin
        if not Ini.SectionExists('SH') then
           fsDtHrArquivo := 0
        else
        begin
           fsIdentPAF.Empresa.Cep         := Ini.ReadString('SH','Cep','');
           fsIdentPAF.Empresa.Cidade      := Ini.ReadString('SH','Cidade','');
           fsIdentPAF.Empresa.CNPJ        := Ini.ReadString('SH','CNPJ','');
           fsIdentPAF.Empresa.Contato     := Ini.ReadString('SH','Contato','');
           fsIdentPAF.Empresa.Email       := Ini.ReadString('SH','Email','');
           fsIdentPAF.Empresa.Endereco    := Ini.ReadString('SH','Endereco','');
           fsIdentPAF.Empresa.IE          := Ini.ReadString('SH','IE','');
           fsIdentPAF.Empresa.IM          := Ini.ReadString('SH','IM','');
           fsIdentPAF.Empresa.RazaoSocial := Ini.ReadString('SH','Nome','');
           fsIdentPAF.Empresa.Telefone    := Ini.ReadString('SH','Telefone','');
           fsIdentPAF.Empresa.Uf          := Ini.ReadString('SH','Uf','');
        end;
     end ;

     if (not ArquivoInvalido) and GravarDadosPAF then
     begin
        if not Ini.SectionExists('PAF') then
           fsDtHrArquivo := 0
        else
        begin
           fsIdentPAF.NumeroLaudo             := Ini.ReadString('PAF','NumeroLaudo','');        // Número do Laudo
           fsIdentPAF.DataLaudo               := Ini.ReadDate('PAF','EmissaoLaudo',0);          // Emissão do Laudo
           fsIdentPAF.NumeroCredencimento     := Ini.ReadString('PAF','NumeroCredencimento','');// Número do Laudo
           fsIdentPAF.VersaoER                := Ini.ReadString('PAF','VersaoER','');           // Versão do Roteiro Executado na Homologação
           fsIdentPAF.Paf.Nome                := Ini.ReadString('PAF','Nome','');               // Nome do Sistema PAF
           fsIdentPAF.Paf.Versao              := Ini.ReadString('PAF','Versao','');             // Versão do Sistema PAF
           fsIdentPAF.Paf.Linguagem           := Ini.ReadString('PAF','Linguagem','');          // Linguagem de programação utilizada
           fsIdentPAF.Paf.BancoDados          := Ini.ReadString('PAF','BancoDados','');         // Banco de dados utilizado
           fsIdentPAF.Paf.SistemaOperacional  := Ini.ReadString('PAF','SistemaOperacional',''); // Sistema operacional em que roda o aplicativo
           fsIdentPAF.Paf.PrincipalExe.Nome   := Ini.ReadString('PAF','NomeExe','');            // Nome do Principal EXE do PAF
           fsIdentPAF.Paf.PrincipalExe.MD5    := Ini.ReadString('PAF','MD5Exe','');             // MD5 do Principal EXE do PAF

           fsIdentPAF.Paf.TipoFuncionamento   := TACBrPAFTipoFuncionamento(Ini.ReadInteger('PAF', 'TipoFuncionamento', 0));
           fsIdentPAF.Paf.TipoDesenvolvimento := TACBrPAFTipoDesenvolvimento(Ini.ReadInteger('PAF', 'TipoDesenvolvimento', 0));
           fsIdentPAF.Paf.IntegracaoPAFECF    := TACBrPAFTipoIntegracao(Ini.ReadInteger('PAF', 'IntegracaoPAFECF', 0));

           fsIdentPAF.Paf.PerfilRequisitos    := Ini.ReadString('PAF', 'PerfilRequisitos', '');
           fsIdentPAF.Paf.UFContribuinte      := Ini.ReadString('PAF', 'UFContribuinte', '');  // Sigla da UF do Contribuinte
        end ;
     end ;

     if (not ArquivoInvalido) and GravarConfigApp then
     begin
        if not Ini.SectionExists('PAF') then
           fsDtHrArquivo := 0
        else
        begin
           fsIdentPAF.Paf.RealizaPreVenda              := Ini.ReadBool('PAF', 'RealizaPreVenda', False);
           fsIdentPAF.Paf.RealizaDAVECF                := Ini.ReadBool('PAF', 'RealizaDAVECF', False);
           fsIdentPAF.Paf.RealizaDAVNaoFiscal          := Ini.ReadBool('PAF', 'RealizaDAVNaoFiscal', False);
           fsIdentPAF.Paf.RealizaDAVOS                 := Ini.ReadBool('PAF', 'RealizaDAVOS', False);
           fsIdentPAF.Paf.DAVConfAnexoII               := Ini.ReadBool('PAF', 'DAVConfAnexoII', False);
           fsIdentPAF.Paf.RealizaLancamentoMesa        := Ini.ReadBool('PAF', 'RealizaLancamentoMesa', False);
           fsIdentPAF.Paf.IndiceTecnicoProd            := Ini.ReadBool('PAF', 'IndiceTecnicoProd', False);
           fsIdentPAF.Paf.BarSimilarECFRestaurante     := Ini.ReadBool('PAF', 'BarSimilarECFRestaurante', False);
           fsIdentPAF.Paf.BarSimilarECFComum           := Ini.ReadBool('PAF', 'BarSimilarECFComum', False);
           fsIdentPAF.Paf.BarSimilarBalanca            := Ini.ReadBool('PAF', 'BarSimilarBalanca', False);
           fsIdentPAF.Paf.UsaImpressoraNaoFiscal       := Ini.ReadBool('PAF', 'UsaImpressoraNaoFiscal', False);
           fsIdentPAF.Paf.DAVDiscrFormula              := Ini.ReadBool('PAF', 'DAVDiscrFormula', False);
           fsIdentPAF.Paf.ImpedeVendaVlrZero           := Ini.ReadBool('PAF', 'ImpedeVendaVlrZero', False);
           fsIdentPAF.Paf.AcumulaVolumeDiario          := Ini.ReadBool('PAF', 'AcumulaVolumeDiario', False);
           fsIdentPAF.Paf.ArmazenaEncerranteIniFinal   := Ini.ReadBool('PAF', 'ArmazenaEncerranteIniFinal', False);
           fsIdentPAF.Paf.EmiteContrEncerrAposREDZLEIX := Ini.ReadBool('PAF', 'EmiteContrEncerrAposREDZLEIX', False);
           fsIdentPAF.Paf.IntegradoComBombas           := Ini.ReadBool('PAF', 'IntegradoComBombas', False);
           fsIdentPAF.Paf.CriaAbastDivergEncerrante    := Ini.ReadBool('PAF', 'CriaAbastDivergEncerrante', False);
           fsIdentPAF.Paf.CadastroPlacaBomba           := Ini.ReadBool('PAF', 'CadastroPlacaBomba', False);
           fsIdentPAF.Paf.TransportePassageiro         := Ini.ReadBool('PAF', 'TransportePassageiro', False);
           fsIdentPAF.Paf.TotalizaValoresLista         := Ini.ReadBool('PAF', 'TotalizaValoresLista', False);
           fsIdentPAF.Paf.TransfPreVenda               := Ini.ReadBool('PAF', 'TransfPreVenda', False);
           fsIdentPAF.Paf.TransfDAV                    := Ini.ReadBool('PAF', 'TransfDAV', False);
           fsIdentPAF.Paf.RecompoeGT                   := Ini.ReadBool('PAF', 'RecompoeGT', False);
           fsIdentPAF.Paf.RecompoeNumSerie             := Ini.ReadBool('PAF', 'RecompoeNumSerie', False);
           fsIdentPAF.Paf.EmitePED                     := Ini.ReadBool('PAF', 'EmitePED', False);
           fsIdentPAF.Paf.CupomMania                   := Ini.ReadBool('PAF', 'CupomMania', False);
           fsIdentPAF.Paf.MinasLegal                   := Ini.ReadBool('PAF', 'MinasLegal', False);
           fsIdentPAF.Paf.NotaLegalDF                  := Ini.ReadBool('PAF', 'NotaLegalDF', False);
           fsIdentPAF.Paf.ParaibaLegal                 := Ini.ReadBool('PAF', 'ParaibaLegal', False);
           fsIdentPAF.Paf.TrocoEmCartao                := Ini.ReadBool('PAF', 'TrocoEmCartao', False);
        end;
     end;

     if ArquivoInvalido then
       raise EACBrAAC_ArquivoInvalido.Create(
          ACBrStr('Arquivo: '+NomeArquivoAux+' inválido') );

     fsIdentPAF.ArquivoListaAutenticados.MD5 := Ini.ReadString('PAF','MD5','');     // MD5 do arquivo que contem a lista de arquivos autenticados

     fsIdentPAF.ECFsAutorizados.Clear;
     I := 0 ;
     while True do
     begin
        Ident := 'ECF_'+IntToStrZero(I,4);
        Linha := Ini.ReadString( 'ECFs', Ident, '*FIM*' );

        if Linha = '*FIM*' then
           break ;

        with fsIdentPAF.ECFsAutorizados.New do
        begin
           LinhaDados := Linha;
        end;

        Inc( I ) ;
     end ;

     Params.Clear;
     I := 0 ;
     while True do
     begin
        Ident := 'L_'+IntToStrZero(I,4);
        Linha := Ini.ReadString( 'Params', Ident, '*FIM*' );

        if Linha = '*FIM*' then
           break ;

        Params.Add( Linha );
        Inc( I ) ;
     end ;

     if Assigned( fsOnDepoisAbrirArquivo ) then
        fsOnDepoisAbrirArquivo( Self );

  finally
     Ini.Free ;
     SL.Free;
  end ;
end ;

procedure TACBrAAC.SalvarArquivo ;
var
  Ini: TMemIniFile;
  SL : TStringList;
  I  : Integer ;
  Ident  : String ;
  ArqBak : String ;
  R      : AnsiString ;
  Continua : Boolean ;
  CRC : Word;
begin
  GravaLog( 'GravarArqRegistro' );

  Continua := True;
  if Assigned( fsOnAntesGravarArquivo ) then
    fsOnAntesGravarArquivo( Continua );

  if not Continua then
  begin
    GravaLog( 'GravarArqRegistro abortado' );
    exit;
  end ;

  if NomeArquivoAux = '' then
    raise EACBrAAC_SemNomeArquivo.Create( ACBrStr('Nome do Arquivo não Informado em: ACBrAAC.NomeArquivoAux') ) ;

  if GravarDadosSH then
  begin
    if (fsIdentPAF.Empresa.RazaoSocial = '') or (fsIdentPAF.Empresa.CNPJ = '') then
      raise EACBrAAC_ArquivoInvalido.Create(
         ACBrStr('SH_RazaoSocial e/ou SH_CNPJ não informados') );
  end ;

  if GravarDadosPAF then
  begin
    if (fsIdentPAF.Paf.Nome = '') or (fsIdentPAF.Paf.Versao = '')then
      raise EACBrAAC_ArquivoInvalido.Create(
         ACBrStr('PAF_Nome e/ou PAF_Versao não informados') );
  end ;

  SL  := TStringList.Create ;
  Ini := TMemIniFile.Create( '' ) ;
  try
     if GravarDadosSH then
     begin
        Ini.WriteString('SH','CNPJ',fsIdentPAF.Empresa.CNPJ);
        Ini.WriteString('SH','Nome',fsIdentPAF.Empresa.RazaoSocial);
        Ini.WriteString('SH','Cep',fsIdentPAF.Empresa.Cep);
        Ini.WriteString('SH','Cidade',fsIdentPAF.Empresa.Cidade);
        Ini.WriteString('SH','Contato',fsIdentPAF.Empresa.Contato);
        Ini.WriteString('SH','Email',fsIdentPAF.Empresa.Email);
        Ini.WriteString('SH','Endereco',fsIdentPAF.Empresa.Endereco);
        Ini.WriteString('SH','IE',fsIdentPAF.Empresa.IE);
        Ini.WriteString('SH','IM',fsIdentPAF.Empresa.IM);
        Ini.WriteString('SH','Telefone',fsIdentPAF.Empresa.Telefone);
        Ini.WriteString('SH','Uf',fsIdentPAF.Empresa.Uf);
     end ;

     if GravarDadosPAF then
     begin
        Ini.WriteString('PAF','Nome',fsIdentPAF.Paf.Nome);                 // Nome do Sistema PAF
        Ini.WriteString('PAF','Versao',fsIdentPAF.Paf.Versao);             // Versão do Sistema PAF

        Ini.WriteString('PAF','Linguagem',fsIdentPAF.Paf.Linguagem);                   // Linguagem de programação utilizada
        Ini.WriteString('PAF','BancoDados',fsIdentPAF.Paf.BancoDados);                 // Banco de dados utilizado
        Ini.WriteString('PAF','SistemaOperacional',fsIdentPAF.Paf.SistemaOperacional); // Sistema operacional no qual roda o aplicativo

        Ini.WriteString('PAF','NumeroLaudo',fsIdentPAF.NumeroLaudo);       // Número do Laudo
        Ini.WriteDate('PAF','EmissaoLaudo',fsIdentPAF.DataLaudo);       // Emissão do Laudo
        Ini.WriteString('PAF','NumeroCredencimento',fsIdentPAF.NumeroCredencimento);       // Número do Laudo
        Ini.WriteString('PAF','VersaoER',fsIdentPAF.VersaoER);             // Versão do Roteiro Executado na Homologação
        Ini.WriteString('PAF','NomeExe',fsIdentPAF.Paf.PrincipalExe.Nome); // Nome do Principal EXE do PAF
        Ini.WriteString('PAF','MD5Exe',fsIdentPAF.Paf.PrincipalExe.MD5);   // MD5  do Principal EXE do PAF

        Ini.WriteInteger('PAF', 'TipoFuncionamento', Integer(fsIdentPAF.Paf.TipoFuncionamento));
        Ini.WriteInteger('PAF', 'TipoDesenvolvimento', Integer(fsIdentPAF.Paf.TipoDesenvolvimento));
        Ini.WriteInteger('PAF', 'IntegracaoPAFECF', Integer(fsIdentPAF.Paf.IntegracaoPAFECF));

        Ini.WriteString('PAF', 'PerfilRequisitos', fsIdentPAF.Paf.PerfilRequisitos);
        Ini.WriteString('PAF', 'UFContribuinte', fsIdentPAF.Paf.UFContribuinte);  // Sigla da UF do Contribuinte
     end ;

     if GravarConfigApp then
     begin
        Ini.WriteBool('PAF', 'RealizaPreVenda', fsIdentPAF.Paf.RealizaPreVenda);
        Ini.WriteBool('PAF', 'RealizaDAVECF', fsIdentPAF.Paf.RealizaDAVECF);
        Ini.WriteBool('PAF', 'RealizaDAVNaoFiscal', fsIdentPAF.Paf.RealizaDAVNaoFiscal);
        Ini.WriteBool('PAF', 'RealizaDAVOS', fsIdentPAF.Paf.RealizaDAVOS);
        Ini.WriteBool('PAF', 'DAVConfAnexoII', fsIdentPAF.Paf.DAVConfAnexoII);
        Ini.WriteBool('PAF', 'RealizaLancamentoMesa', fsIdentPAF.Paf.RealizaLancamentoMesa);
        Ini.WriteBool('PAF', 'IndiceTecnicoProd', fsIdentPAF.Paf.IndiceTecnicoProd);
        Ini.WriteBool('PAF', 'BarSimilarECFRestaurante', fsIdentPAF.Paf.BarSimilarECFRestaurante);
        Ini.WriteBool('PAF', 'BarSimilarECFComum', fsIdentPAF.Paf.BarSimilarECFComum);
        Ini.WriteBool('PAF', 'BarSimilarBalanca', fsIdentPAF.Paf.BarSimilarBalanca);
        Ini.WriteBool('PAF', 'UsaImpressoraNaoFiscal', fsIdentPAF.Paf.UsaImpressoraNaoFiscal);
        Ini.WriteBool('PAF', 'DAVDiscrFormula', fsIdentPAF.Paf.DAVDiscrFormula);
        Ini.WriteBool('PAF', 'ImpedeVendaVlrZero', fsIdentPAF.Paf.ImpedeVendaVlrZero);
        Ini.WriteBool('PAF', 'AcumulaVolumeDiario', fsIdentPAF.Paf.AcumulaVolumeDiario);
        Ini.WriteBool('PAF', 'ArmazenaEncerranteIniFinal', fsIdentPAF.Paf.ArmazenaEncerranteIniFinal);
        Ini.WriteBool('PAF', 'EmiteContrEncerrAposREDZLEIX', fsIdentPAF.Paf.EmiteContrEncerrAposREDZLEIX);
        Ini.WriteBool('PAF', 'IntegradoComBombas', fsIdentPAF.Paf.IntegradoComBombas);
        Ini.WriteBool('PAF', 'CriaAbastDivergEncerrante', fsIdentPAF.Paf.CriaAbastDivergEncerrante);
        Ini.WriteBool('PAF', 'CadastroPlacaBomba', fsIdentPAF.Paf.CadastroPlacaBomba);
        Ini.WriteBool('PAF', 'TransportePassageiro', fsIdentPAF.Paf.TransportePassageiro);
        Ini.WriteBool('PAF', 'TotalizaValoresLista', fsIdentPAF.Paf.TotalizaValoresLista);
        Ini.WriteBool('PAF', 'TransfPreVenda', fsIdentPAF.Paf.TransfPreVenda);
        Ini.WriteBool('PAF', 'TransfDAV', fsIdentPAF.Paf.TransfDAV);
        Ini.WriteBool('PAF', 'RecompoeGT', fsIdentPAF.Paf.RecompoeGT);
        Ini.WriteBool('PAF', 'RecompoeNumSerie', fsIdentPAF.Paf.RecompoeNumSerie);
        Ini.WriteBool('PAF', 'EmitePED', fsIdentPAF.Paf.EmitePED);
        Ini.WriteBool('PAF', 'CupomMania', fsIdentPAF.Paf.CupomMania);
        Ini.WriteBool('PAF', 'MinasLegal', fsIdentPAF.Paf.MinasLegal);
        Ini.WriteBool('PAF', 'NotaLegalDF', fsIdentPAF.Paf.NotaLegalDF);
        Ini.WriteBool('PAF', 'ParaibaLegal', fsIdentPAF.Paf.ParaibaLegal);
        Ini.WriteBool('PAF', 'TrocoEmCartao', fsIdentPAF.Paf.TrocoEmCartao);
     end;


     Ini.WriteString('PAF','MD5',fsIdentPAF.ArquivoListaAutenticados.MD5); // MD5 do arquivo que contem a lista de arquivos autenticados

     // Lista de ECFs autorizados a usar o PAF-ECF no estabelecimento
     For I := 0 to fsIdentPAF.ECFsAutorizados.Count-1 do
     begin
        Ident := 'ECF_'+IntToStrZero(I,4);
        Ini.WriteString( 'ECFs', Ident, fsIdentPAF.ECFsAutorizados[I].LinhaDados );
     end ;

     // Lista de parametros adicionais
     For I := 0 to Params.Count-1 do
     begin
        Ident := 'L_'+IntToStrZero(I,4);
        Ini.WriteString( 'Params', Ident, Params[I] );
     end ;

     // Codigo re-inserido para manter compatibilidade de Arquivo AAC com
     // Executaveis compilados com a versão antiga. Caso contrário Executaveis
     // antigos cairiam no Erro: "Arquivo XXX inválido" ao tentar abrir o arquivo
     if GravarDadosPAF and GravarDadosSH then
     begin
       // Calculando o CRC //
       CRC := StringCrc16( IdentPAF.Empresa.RazaoSocial + IdentPAF.Empresa.CNPJ +
                           IdentPAF.Empresa.IE + IdentPAF.Empresa.IM +
                           IdentPAF.Paf.Nome + IdentPAF.Paf.Versao +
                           fsIdentPAF.Paf.PrincipalExe.MD5 );
       Ini.WriteInteger('CHK','CRC16',CRC);
     end ;

     Ini.GetStrings( SL );
     SL.Add( 'CRC:' + IntToStr( StringCrc16( SL.Text ) ) );

     // DEBUG
     //GravaLog('Arquivo em Memoria: '+sLineBreak+ SL.Text );
     R := Criptografar( SL.Text );
     // DEBUG
     //GravaLog('Arquivo Criptografado: '+sLineBreak+ R );

     if fsCriarBAK then
     begin
        ArqBak := ChangeFileExt( fsNomeCompleto, '.bak');
        SysUtils.DeleteFile( ArqBak );
        RenameFile( fsNomeCompleto, ArqBak );
     end ;

     WriteToTXT( fsNomeCompleto, R, False, False );

     if fsEfetuarFlush then
        FlushFileToDisk( fsNomeCompleto );

     {$IFDEF DELPHI2007_UP}
     FileAge( fsNomeCompleto, fsDtHrArquivo );
     {$ELSE}
     fsDtHrArquivo := FileDateToDateTime( FileAge( fsNomeCompleto ) );
     {$ENDIF}

     if Assigned( fsOnDepoisGravarArquivo ) then
        fsOnDepoisGravarArquivo( Self );
  finally
     Ini.Free ;
     SL.Free;
  end ;
end ;

procedure TACBrAAC.VerificaReCarregarArquivo ;
var
   NewDtHrArquivo : TDateTime;
   Recarregar : Boolean ;
begin
  Recarregar := ArquivoInvalido;
  //GravaLog( 'VerificaReCarregarArquivo_Inicio(Recarregar = '+ BoolToStr(Recarregar, True ) + ' )' );

  if not Recarregar then
  begin
     // Data/Hora do arquivo é diferente ?
     try
       {$IFDEF DELPHI2007_UP}
       FileAge( fsNomeCompleto, NewDtHrArquivo );
       {$ELSE}
       NewDtHrArquivo := FileDateToDateTime( FileAge( fsNomeCompleto ) );
       {$ENDIF}
       Recarregar := (fsDtHrArquivo <> NewDtHrArquivo)
     except
       Recarregar := true;
     end;
  end ;

//  GravaLog( 'VerificaReCarregarArquivo_FIM(Recarregar = '+ BoolToStr(Recarregar, True) + ' )' );

  if Recarregar then
     AbrirArquivo ;
end ;

function TACBrAAC.AchaIndiceECF(const NumeroSerie : String ) : Integer ;
Var
  I : Integer ;
begin
  VerificaReCarregarArquivo;

  I := 0 ;
  while (I < fsIdentPAF.ECFsAutorizados.Count) and
        (NumeroSerie <> fsIdentPAF.ECFsAutorizados[I].NumeroSerie) do
    Inc( I ) ;

  if I = fsIdentPAF.ECFsAutorizados.Count then
     Result := -1
  else
     Result := I ;
end ;

function TACBrAAC.VerificarGTECF(const NumeroSerie : String ;
   var ValorGT : Double) : Integer ;
// Retornos:
//   0 = Tudo OK
//  -1 = NumSerie não encontrado
//  -2 = GT não confere
var
   AECF : TACBrAACECF ;
   ValorGTNovo : Double ;
   CRONovo, CNINovo : Integer ;
   NewECF : TACBrAACECF ;
begin
  GravaLog( 'VerificarGTECF( '+ NumeroSerie+ ', '+FloatToStr(ValorGT) +' )' );

  Result := 0;
//  VerificaReCarregarArquivo; // Não necessário. Já é chamado dentro de AchaECF

  AECF := AchaECF( NumeroSerie );
  if not Assigned( AECF ) then
   begin
     Result := -1;

     if fsIdentPAF.Paf.RecompoeNumSerie and
        Assigned( fsOnVerificarRecomporNumSerie ) then
     begin
        GravaLog( '  OnVerificarRecomporNumSerie' );
        CRONovo := 0;
        CNINovo := 0;

        fsOnVerificarRecomporNumSerie( NumeroSerie, ValorGT, CRONovo, CNINovo ) ;
        if (CRONovo > 0) then
        begin
           GravaLog( '    Recompondo' );
           NewECF := TACBrAACECF.Create;
           NewECF.NumeroSerie    := NumeroSerie;
           NewECF.CRO            := CRONovo;
           NewECF.DtHrAtualizado := Now;
           NewECF.ValorGT        := ValorGT;
           NewECF.CNI            := CNINovo;
           fsIdentPAF.ECFsAutorizados.Add( NewECF );
           SalvarArquivo;

           Result := 0;
        end ;
     end ;
   end
  else
    if RoundTo( AECF.ValorGT, -2) <> RoundTo( ValorGT, -2 ) then
    begin
       ValorGT := AECF.ValorGT;
       Result  := -2;

       if fsIdentPAF.Paf.RecompoeGT and
          Assigned( fsOnVerificarRecomporValorGT ) then
       begin
          GravaLog( '  OnVerificarRecomporValorGT' );

          ValorGTNovo := AECF.ValorGT;
          fsOnVerificarRecomporValorGT( NumeroSerie, ValorGTNovo );

          if RoundTo( ValorGTNovo, -2) <> RoundTo( AECF.ValorGT, -2) then
          begin
             GravaLog( '    Recompondo' );
             AtualizarValorGT( NumeroSerie, ValorGTNovo );
             Result := 0;
          end ;
       end ;
    end ;
end ;

procedure TACBrAAC.AtualizarMD5(const AMD5 : String) ;
var
  iFor: Integer;
begin
  GravaLog( 'AtualizarMD5 - De: '+fsIdentPAF.ArquivoListaAutenticados.MD5+' Para: '+AMD5 );

  VerificaReCarregarArquivo;

  if AMD5 = fsIdentPAF.ArquivoListaAutenticados.MD5 then exit ;

  fsIdentPAF.ArquivoListaAutenticados.MD5 := AMD5;

  // Acha o MD5 do EXE principal, caso o nome já esteja definido
  if fsIdentPAF.Paf.PrincipalExe.Nome <> '' then
  begin
     for iFor := 0 to fsIdentPAF.OutrosArquivos.Count - 1 do
     begin
        if AnsiCompareText(fsIdentPAF.Paf.PrincipalExe.Nome, fsIdentPAF.OutrosArquivos[iFor].Nome) = 0 then
        begin
           fsIdentPAF.Paf.PrincipalExe.MD5 := fsIdentPAF.OutrosArquivos[iFor].MD5;
           Break
        end;
     end;
  end;

  SalvarArquivo;
end ;

procedure TACBrAAC.AtualizarValorGT(const NumeroSerie : String ;
   const ValorGT : Double) ;
var
  AECF, NewECF : TACBrAACECF ;
  LogTXT: String;
begin
  LogTXT := 'AtualizarGTECF - NumSerie: '+NumeroSerie ;

//  GravaLog( LogTXT +' Entrada'); //Debug

  AECF := AchaECF( NumeroSerie );    // AchaECF chama VerificaReCarregarArquivo;
  if not Assigned( AECF ) then
  begin
     LogTXT := LogTXT +' - nao encontrado';
     GravaLog( LogTXT );
     raise EACBrAAC_NumSerieNaoEncontrado.Create( ACBrStr(
        'Erro ao atualivar Valor do G.T.'+sLineBreak+
        ' ECF: '+NumeroSerie+' não encontrado') );
  end ;

  if RoundTo( ValorGT, -2) = RoundTo( AECF.ValorGT, -2) then Exit ;

  LogTXT := LogTXT + ' - De:' +FormatFloat('###,###,##0.00',AECF.ValorGT)+
                     ' Para:'+FormatFloat('###,###,##0.00',ValorGT) ;
  GravaLog( LogTXT );

  AECF.ValorGT        := ValorGT ;
  AECF.DtHrAtualizado := Now;

  if (not fsGravarTodosECFs) and (fsIdentPAF.ECFsAutorizados.Count > 1) then
  begin
    NewECF := TACBrAACECF.Create;
    NewECF.NumeroSerie    := AECF.NumeroSerie;
    NewECF.CRO            := AECF.CRO;
    NewECF.DtHrAtualizado := AECF.DtHrAtualizado;
    NewECF.ValorGT        := AECF.ValorGT;   
    NewECF.CNI            := AECF.CNI;

    fsIdentPAF.ECFsAutorizados.Clear;
    fsIdentPAF.ECFsAutorizados.Add( NewECF );
  end ;

  SalvarArquivo;
end ;

procedure TACBrAAC.GravaLog( const AString : AnsiString) ;
begin
  if fsArqLOG = '' then
     exit ;

  try
     WriteToTXT(fsArqLOG, FormatDateTime('dd/mm hh:nn:ss:zzz',Now)+' - '+AString, True);
  except
  end ;
end ;

function TACBrAAC.GetArquivoInvalido: Boolean;
begin
   Result := (fsDtHrArquivo <= 0);
end;

function TACBrAAC.GetChave : AnsiString ;
Var
  AChave : AnsiString ;
begin
  AChave := '';
  if Assigned( fsOnGetChave ) then
     fsOnGetChave( AChave );

  if AChave = '' then
     raise EACBrAAC_SemChave.Create(
        ACBrStr('Chave não informada ou Evento ACBrAAC.OnGetChave não programado' ) );

  Result := AChave;
end;

procedure TACBrAAC.SetNomeArquivoAux(const AValue : String) ;
begin
  if fsNomeArquivoAux = AValue then exit ;

  fsNomeArquivoAux := AValue ;

  fsNomeCompleto := fsNomeArquivoAux;
  // Tem Path no Nome do Arquivo ?
  if (fsNomeCompleto <> '') and (pos(PathDelim, fsNomeCompleto) = 0) then
     fsNomeCompleto := ExtractFilePath( ParamStr(0) ) + fsNomeCompleto;
end ;

procedure TACBrAAC.SetParams(const AValue : TStringList) ;
begin
   fsParams.Assign( AValue );
end ;

function TACBrAAC.Criptografar( const Dados : AnsiString) : AnsiString ;
var
   R : AnsiString ;
begin
  R := '' ;
  if Assigned( fsOnCrypt ) then
   begin
     fsOnCrypt( Dados, R ) ;

     if R = '' then
        raise EACBrAAC_SemResposta.Create(
           ACBrStr('Evento ACBrACC.OnCrypt não informou a resposta') ) ;
   end
  else
     R := StrCrypt( Dados, Chave );

  Result := R ;
end ;

function TACBrAAC.DesCriptografar( const Dados : AnsiString) : AnsiString ;
var
   R : AnsiString ;
begin
  R := '' ;
  if Assigned( fsOnDeCrypt ) then
   begin
     fsOnDeCrypt( Dados, R ) ;

     if R = '' then
        raise EACBrAAC_SemResposta.Create(
           ACBrStr('Evento ACBrACC.OnDeCrypt não informou a resposta') ) ;
   end
  else
     R := StrCrypt( Dados, Chave );

  Result := R ;
end ;

end.

