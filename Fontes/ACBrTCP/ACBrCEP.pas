{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Victor Hugo Gonzales - Pandaaa                  }
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

unit ACBrCEP;

{$I ACBr.inc}

interface

uses
  Classes,
  SysUtils,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections,
   System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase,
  ACBrSocket,
  ACBrIBGE;

type
  TACBrCEPWebService = ( wsNenhum,
                         wsBuscarCep,
                         wsCepLivre,
                         wsRepublicaVirtual,
                         wsBases4you,
                         wsRNSolucoes,
                         wsKingHost,
                         wsByJG,
                         wsCorreios,
                         wsDevMedia,
                         wsViaCep,
                         wsCorreiosSIGEP,
                         wsCepAberto,
                         wsWSCep,
                         wsOpenCep,
                         wsBrasilAPI );

  EACBrCEPException = class ( Exception );

  { TACBrCEPEndereco }

  TACBrCEPEndereco = class
    private
      fBairro          : String;
      fCEP             : string;
      fCodigoIBGE      : string;
      fComplemento     : string;
      fLogradouro      : string;
      fMunicipio       : string;
      fTipo_Logradouro : string;
      fUF              : string;
      fAltitude        : string;
      fLatitude        : string;
      fLongitude       : string;

      function GetIBGE_UF : string;
    public
      constructor Create ;

      property CEP             : string read fCEP             write fCEP ;
      property Tipo_Logradouro : string read fTipo_Logradouro write fTipo_Logradouro ;
      property Logradouro      : string read fLogradouro      write fLogradouro ;
      property Complemento     : string read fComplemento     write fComplemento ;
      property Bairro          : string read fBairro          write fBairro ;
      property Municipio       : string read fMunicipio       write fMunicipio ;
      property UF              : string read fUF              write fUF ;
      property IBGE_Municipio  : string read fCodigoIBGE      write fCodigoIBGE ;
      property IBGE_UF         : string read GetIBGE_UF;
      property Altitude        : string read fAltitude        write fAltitude ;
      property Latitude        : string read fLatitude        write fLatitude ;
      property Longitude       : string read fLongitude       write fLongitude ;
  end ;

  { Lista de Objetos do tipo TACBrCEPEndereco }

  { TACBrCEPEnderecos }

  TACBrCEPEnderecos = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrCEPEndereco>{$EndIf})
    protected
      procedure SetObject (Index: Integer; Item: TACBrCEPEndereco);
      function  GetObject (Index: Integer): TACBrCEPEndereco;
      procedure Insert    (Index: Integer; Obj: TACBrCEPEndereco);
    public
      function Add     (Obj: TACBrCEPEndereco): Integer;
      function New:    TACBrCEPEndereco ;
      property Objects [Index: Integer]: TACBrCEPEndereco read GetObject write SetObject; default;
    end;

  TACBrCEPWSClass = class;

  { TACBrCEP }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCEP = class( TACBrHTTP )
    private
      fWebService      : TACBrCEPWebService;
      fACBrCEPWS       : TACBrCEPWSClass;
      fEnderecos       : TACBrCEPEnderecos;
      fOnBuscaEfetuada : TNotifyEvent;
      fChaveAcesso     : string;
      fUsuario         : string;
      fSenha           : string;
      FPesquisarIBGE   : Boolean;
      function  GetURL : string ;
      procedure SetWebService (const AValue : TACBrCEPWebService) ;
    public
      constructor Create (AOwner: TComponent); override;
      Destructor Destroy; override;

      property Enderecos : TACBrCEPEnderecos  read fEnderecos;

      function BuscarPorCEP( ACEP : String ) : Integer ;
      function BuscarPorLogradouro( const ACidade, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) : Integer ;

    published
      property WebService      : TACBrCEPWebService read fWebService    write SetWebService default wsNenhum ;
      property URL             : String             read GetURL ;
      property ChaveAcesso     : String             read fChaveAcesso   write fChaveAcesso ;
      property Usuario         : String             read fUsuario       write fUsuario ;
      property Senha           : String             read fSenha         write fSenha ;
      property PesquisarIBGE   : Boolean            read FPesquisarIBGE write FPesquisarIBGE; // Válido somente para wsCorreios e TACBrWSCorreiosSIGEP
      property OnBuscaEfetuada : TNotifyEvent       read fOnBuscaEfetuada
         write fOnBuscaEfetuada ;
  end ;

  { TACBrCEPWSClass }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCEPWSClass = class
    private
      fOwner : TACBrCEP ;
      fpURL : String ;

      procedure ErrorAbstract(NomeProcedure: String);
    protected
      procedure TestarChave;
      procedure TestarUsuario;
      procedure BuscaEfetuada;
    public
      constructor Create( AOwner : TACBrCEP ) ; virtual ;

      Procedure BuscarPorCEP( const ACEP : String ) ; virtual ;
      Procedure BuscarPorLogradouro( const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; virtual ;

      property URL : String read fpURL ;
  end ;

  { TACBrWSBuscarCEP }

  TACBrWSBuscarCEP = class(TACBrCEPWSClass)
    private
      procedure ProcessaResposta ;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP(const ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSCEPLivre }

  TACBrWSCEPLivre = class(TACBrCEPWSClass)
    private
      procedure ProcessaResposta ;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP(const ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro(const  AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSRepublicaVirtual }

  TACBrWSRepublicaVirtual = class(TACBrCEPWSClass)
    private
      FCepBusca: String;
      procedure ProcessaResposta ;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP(const ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  TACBrWSBases4you = class(TACBrCEPWSClass)
    private
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP(const ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  TACBrWSRNSolucoes = class(TACBrCEPWSClass)
  private
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;

    Procedure BuscarPorCEP(const ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end;

  TACBrWSKingHost = class(TACBrCEPWSClass)
  private
    FCepBusca: String;
    procedure ProcessaResposta ;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;

    Procedure BuscarPorCEP(const ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSByJG }

  TACBrWSByJG = class(TACBrCEPWSClass)
  private
    FCepBusca: String;
    FTipoBusca: Integer;
    procedure ProcessaCEP;
    procedure ProcessaLogradouro;
    procedure ProcessaResposta ;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;

    Procedure BuscarPorCEP(const ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  { TACBrDevMedia }

  TACBrWSDevMedia = class(TACBrCEPWSClass)
    private
      FCepBusca: String;
      procedure ProcessaResposta;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP(const ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSViaCEP }
  TACBrWSViaCEP = class(TACBrCEPWSClass)
  private
    FCepBusca: String;
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ); override;

    Procedure BuscarPorCEP(const ACEP : String ); override;
    Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,ALogradouro, AUF, ABairro : String ); override;
  end;

  { TACBrWSCorreiosSIGEP }

  TACBrWSCorreiosSIGEP = class(TACBrCEPWSClass)
  private
    fACBrIBGE: TACBrIBGE;
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;
    destructor Destroy; override;
    Procedure BuscarPorCEP(const ACEP : String ) ; override ;
  end;

  { TACBrWSCEPAberto }

  TACBrWSCEPAberto= class(TACBrCEPWSClass)
  private
    procedure ProcessaResposta ;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;
    Procedure BuscarPorCEP(const ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,ALogradouro, AUF, ABairro : String ); override;
  end ;

  { TACBrWSWSCEP } // WSCep é o nome do servico

  TACBrWSWSCEP = class(TACBrCEPWSClass)
  private
    procedure ProcessaResposta ;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;
    Procedure BuscarPorCEP(const ACEP : String ) ; override ;
  end ;

  { TACBrWSOpenCEP }  // https://github.com/SeuAliado/OpenCEP
  TACBrWSOpenCEP = class(TACBrCEPWSClass)
  private
    FCepBusca: String;
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ); override;

    Procedure BuscarPorCEP(const ACEP : String ); override;
    Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,ALogradouro, AUF, ABairro : String ); override;
  end;

  { TACBrWSBrasilAPI } // https://brasilapi.com.br/docs
  TACBrWSBrasilAPI = class(TACBrCEPWSClass)
  private
    FCepBusca: String;
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ); override;

    Procedure BuscarPorCEP(const ACEP : String ); override;
    Procedure BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,ALogradouro, AUF, ABairro : String ); override;
  end;

implementation
uses
  strutils,
  math,
  synacode,
  synautil,
  ACBrUtil.Base,
  ACBrConsts,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrJSON;

  CONST ERROR_METODO_NAO_DISPONIVEL_WS         = 'WebService %s não implementa o método: %s!';
  CONST ERROR_CEP_OBRIGATORIO                  = 'CEP é obrigatório e deve ser informado!';
  CONST ERROR_MUNICIPIO_OBRIGATORIO            = 'Município é obrigatório e deve ser informado!';
  CONST ERROR_UF_OBRIGATORIO                   = 'UF é obrigatório e deve ser informado!';
  CONST ERROR_LOGRADOURO_OBRIGATORIO           = 'Logradouro é obrigatório e deve ser informado!';
  CONST ERROR_LOGRADOURO_CIDADE_UF_OBRIGATORIO = 'Logradouro, Cidade e UF é obrigatório e deve ser informado!';
  CONST ERROR_API_KEY                          = 'Chave de Autenticação é obrigatório e deve ser informado!';
  CONST ERROR_USUARIO                          = 'Usuario é obrigatório e deve ser informado!';
  CONST ERROR_SENHA                            = 'Senha é obrigatório e deve ser informado!';
  CONST ERROR_CEP_NAO_ENCONTRADO               = 'CEP Não foi encontrado.';

{ TACBrCEPEndereco ************************************************************}

constructor TACBrCEPEndereco.Create ;
begin
  inherited ;

  fCEP             := '' ;
  fTipo_Logradouro := '' ;
  fLogradouro      := '' ;
  fBairro          := '' ;
  fCodigoIBGE      := '' ;
  fMunicipio       := '' ;
  fUF              := '' ;
end ;

function TACBrCEPEndereco.GetIBGE_UF : String ;
begin
  Result := copy(IBGE_Municipio,1,2) ;
end;

{ TACBrCEPEnderecos ***********************************************************}

procedure TACBrCEPEnderecos.SetObject(Index : Integer ; Item : TACBrCEPEndereco) ;
begin
  inherited Items[Index] := Item;
end ;

function TACBrCEPEnderecos.GetObject(Index : Integer) : TACBrCEPEndereco ;
begin
  Result := TACBrCEPEndereco(inherited Items[Index]);
end ;

procedure TACBrCEPEnderecos.Insert(Index : Integer ; Obj : TACBrCEPEndereco) ;
begin
  inherited Insert(Index, Obj);
end ;

function TACBrCEPEnderecos.New: TACBrCEPEndereco;
begin
  Result := TACBrCEPEndereco.Create;
  Self.Add(Result);
end;

function TACBrCEPEnderecos.Add(Obj : TACBrCEPEndereco) : Integer ;
begin
  Result := inherited Add(Obj) ;
end ;

{ TACBrCEP ********************************************************************}

constructor TACBrCEP.Create(AOwner : TComponent) ;
begin
  inherited Create(AOwner) ;

  fOnBuscaEfetuada := nil ;

  fEnderecos     := TACBrCEPEnderecos.create( true );
  fACBrCEPWS     := TACBrCEPWSClass.Create( Self );
  fWebService    := wsNenhum ;
  FPesquisarIBGE := true;
end ;

destructor TACBrCEP.Destroy ;
begin
  fEnderecos.Free;
  fACBrCEPWS.Free;

  inherited Destroy ;
end ;

procedure TACBrCEP.SetWebService(const AValue : TACBrCEPWebService) ;
begin
  if fWebService = AValue then exit ;

  fACBrCEPWS.Free;

  case AValue of
    wsBuscarCep        : fACBrCEPWS := TACBrWSBuscarCEP.Create( Self );
    wsCepLivre         : fACBrCEPWS := TACBrWSCEPLivre.Create( Self );
    wsRepublicaVirtual : fACBrCEPWS := TACBrWSRepublicaVirtual.Create(Self);
    wsBases4you        : fACBrCEPWS := TACBrWSBases4you.Create(Self);
    wsRNSolucoes       : fACBrCEPWS := TACBrWSRNSolucoes.Create(Self);
    wsKingHost         : fACBrCEPWS := TACBrWSKingHost.Create(Self);
    wsByJG             : fACBrCEPWS := TACBrWSByJG.Create(Self);
    //wsCorreios       : fACBrCEPWS := TACBrWSCorreios.Create(Self);      // WebService antigo do correios morreu :(  utiliza SIGEP agora
    wsDevMedia         : fACBrCEPWS := TACBrWSDevMedia.Create(Self);
    wsViaCep           : fACBrCEPWS := TACBrWSViaCEP.Create(Self);
    wsCorreios,
    wsCorreiosSIGEP    : fACBrCEPWS := TACBrWSCorreiosSIGEP.Create(Self);
    wsCepAberto        : fACBrCEPWS := TACBrWSCEPAberto.Create(Self);
    wsWSCep            : fACBrCEPWS := TACBrWSWSCEP.Create(Self);
    wsOpenCep          : fACBrCEPWS := TACBrWSOpenCEP.Create(Self);
    wsBrasilAPI        : fACBrCEPWS := TACBrWSBrasilAPI.Create(Self);
  else
     fACBrCEPWS := TACBrCEPWSClass.Create( Self ) ;
  end ;

  fWebService := AValue;
end;

function TACBrCEP.GetURL : String ;
begin
  Result := fACBrCEPWS.URL ;
end;

function TACBrCEP.BuscarPorCEP(ACEP : String) : Integer ;
begin
  fEnderecos.Clear;

  ACEP := Trim( OnlyNumber( ACEP ) ) ;
  if ACEP = '' then
     raise EACBrCEPException.Create( ERROR_CEP_OBRIGATORIO );

  fACBrCEPWS.BuscarPorCEP(ACEP);

  Result := fEnderecos.Count;
end ;

function TACBrCEP.BuscarPorLogradouro(const ACidade, ATipo_Logradouro, ALogradouro, AUF, ABairro : String ) : Integer ;
begin
  fEnderecos.Clear;
  fACBrCEPWS.BuscarPorLogradouro( ACidade,
                                  ATipo_Logradouro,
                                  ALogradouro,
                                  AUF,
                                  ABairro );
  Result := fEnderecos.Count;
end ;

{ TACBrCEPWSClass *************************************************************}

procedure TACBrCEPWSClass.ErrorAbstract(NomeProcedure: String);
begin
  raise EACBrCEPException.create(ACBrStr(Format('WebService %s não implementa o método: %s', [Self.ClassName, NomeProcedure] ))) ;
end ;

procedure TACBrCEPWSClass.TestarChave;
begin
  if fOwner.ChaveAcesso = EmptyStr then
    raise EACBrCEPException.Create( ERROR_API_KEY );
end;

procedure TACBrCEPWSClass.TestarUsuario;
begin
  if fOwner.Usuario = EmptyStr then
    raise EACBrCEPException.Create( ERROR_USUARIO )
  else if fOwner.Senha = EmptyStr then
    raise EACBrCEPException.Create( ERROR_SENHA );
end;

constructor TACBrCEPWSClass.Create( AOwner : TACBrCEP) ;
begin
  inherited Create ;
  fOwner := AOwner;
  fpURL  := '';
end ;

procedure TACBrCEPWSClass.BuscaEfetuada;
begin
  if Assigned( fOwner.OnBuscaEfetuada ) then
    fOwner.OnBuscaEfetuada( Self );
end;

procedure TACBrCEPWSClass.BuscarPorCEP(const ACEP: String);
begin
  ErrorAbstract( 'BuscarPorCEP' );
end ;

procedure TACBrCEPWSClass.BuscarPorLogradouro(const AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
begin
  ErrorAbstract( 'BuscarPorLogradouro' );
end ;

{ TACBrWSBuscarCEP - http://www.buscarcep.com.br *******************************}

constructor TACBrWSBuscarCEP.Create(AOwner : TACBrCEP) ;
begin
  inherited Create(AOwner) ;
  fpURL := 'http://www.buscarcep.com.br/' ;
end ;

Procedure TACBrWSBuscarCEP.BuscarPorCEP(const ACEP : String ) ;
begin
  TestarChave;

  fOwner.HTTPGet( fpURL + '?cep='+ACEP+'&formato=string&chave='+fOwner.ChaveAcesso ) ;
  ProcessaResposta ;
end ;

procedure TACBrWSBuscarCEP.BuscarPorLogradouro(const AMunicipio,
    ATipo_Logradouro, ALogradouro, AUF, ABairro : String);
Var
  LParams : string ;
  LMunicipio,  LTipoLogradouro, LLogradouro, LUF: string;
begin
  TestarChave;

  LMunicipio       := fOwner.AjustaParam( AMunicipio );
  LTipoLogradouro  := fOwner.AjustaParam( ATipo_Logradouro );
  LLogradouro      := fOwner.AjustaParam( ALogradouro ) ;
  LUF              := fOwner.AjustaParam( AUF );

  if (LMunicipio = '') or (LLogradouro = '') or (LUF = '') then
     raise EACBrCEPException.Create( ERROR_LOGRADOURO_CIDADE_UF_OBRIGATORIO );

  LParams := '?logradouro=' + LLogradouro+
            '&cidade='      + LMunicipio+
            '&uf='          + LUF;

  if LTipoLogradouro <> '' then
    LParams := LParams + '&tipo_logradouro=' + LTipoLogradouro;

  if ABairro <> '' then
    LParams := LParams + '&bairro=' + ABairro;

  LParams := LParams + '&formato=string';
  LParams := LParams + '&chave=' + fOwner.ChaveAcesso;

  fOwner.HTTPGet( fpURL + LParams );
  ProcessaResposta;
end ;

Procedure TACBrWSBuscarCEP.ProcessaResposta ;
Var
   LSL1, LSL2 : TStringList;
   LBuffer    : string;
   LACBrCEPEnderecos : TACBrCEPEndereco;
   LPosIni, I : integer ;
begin
  fOwner.fEnderecos.Clear;

  LSL1 := TStringList.Create;
  LSL2 := TStringList.Create;
  try
    LSL1.Text := StringReplace( fOwner.RespHTTP.Text, '&cep=', sLineBreak+'&cep=', [rfReplaceAll] );

    For I := 0 to Pred( LSL1.Count ) do
    begin
       LBuffer := LSL1[I];
       LPosIni := pos('&cep=',LBuffer);

       if LPosIni > 0 then
       begin

         LBuffer := copy( LBuffer, LPosIni, Length(LBuffer) );

         LSL2.Clear;
         LSL2.Text := StringReplace( LBuffer, '&', sLineBreak, [rfReplaceAll] );

         if (LSL2.Values['resultado'] = '1') then
         begin
            LACBrCEPEnderecos := fOwner.Enderecos.New;

            LACBrCEPEnderecos.CEP             := LSL2.Values['cep'];
            LACBrCEPEnderecos.Tipo_Logradouro := LSL2.Values['tipo_logradouro'];
            LACBrCEPEnderecos.Logradouro      := LSL2.Values['logradouro'];
            LACBrCEPEnderecos.Complemento     := LSL2.Values['complemento'];
            LACBrCEPEnderecos.Bairro          := LSL2.Values['bairro'];
            LACBrCEPEnderecos.Municipio       := LSL2.Values['cidade'];
            LACBrCEPEnderecos.UF              := LSL2.Values['uf'];
            LACBrCEPEnderecos.IBGE_Municipio  := LSL2.Values['ibge_municipio_verificador'];
         end;
       end;
    end;
  finally
    LSL1.free;
    LSL2.free;
  end;

  BuscaEfetuada;
end;

{ TACBrWSCEPLivre - http://ceplivre.com.br/ ***********************************}

constructor TACBrWSCEPLivre.Create(AOwner : TACBrCEP) ;
begin
  inherited Create(AOwner);
  fpURL := 'http://ceplivre.com.br/consultar/';
end;

procedure TACBrWSCEPLivre.BuscarPorCEP(const ACEP : String);
var
  CEP: string;
begin
  TestarChave;

  // CEPLivre exige CEP formatado //
  CEP := Copy(ACEP,1,5)+'-'+Copy(ACEP,6,3);

  fOwner.HTTPGet( fpURL + 'cep/' + Trim(fOwner.ChaveAcesso) + '/' + CEP + '/csv');
  ProcessaResposta ;
end;

Procedure TACBrWSCEPLivre.BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,
  ALogradouro, AUF, ABairro : String) ;
var
  Logradouro: string;
begin
  TestarChave;

  Logradouro := fOwner.AjustaParam( ALogradouro ) ;
  if (Logradouro = '') then
     raise EACBrCEPException.Create('Cidade e Logradouro devem ser informados');

  fOwner.HTTPGet( fpURL + 'logradouro/' + Trim( fOwner.ChaveAcesso ) + '/' + Logradouro + '/csv' ) ;
  ProcessaResposta ;
end;

Procedure TACBrWSCEPLivre.ProcessaResposta;
Var
   LSL1, LSL2 : TStringList;
   LBuffer, LLinha : String;
   LACBrCEPEnderecos : TACBrCEPEndereco;
   I, J : Integer;
begin
  fOwner.fEnderecos.Clear;

  LSL1 := TStringList.Create;
  LSL2 := TStringList.Create;
  try
    LBuffer := fOwner.RespHTTP.Text ;
    // CEP livre retorna vários endereços na mesma linha... tratando...
    LSL1.Text := StringReplace( LBuffer, '""', '"'+sLineBreak+'"', [rfReplaceAll] );

    For I := 0 to Pred( LSL1.Count ) do
    begin
      LBuffer := LSL1[I] ;

      LSL2.Clear;
      LSL2.Text := StringReplace( LBuffer, ',', sLineBreak, [rfReplaceAll] );

      { Removendo as aspas do inicio e fim }
      for J := 0 to Pred( LSL2.Count ) do
      begin
         LLinha := LSL2[J];

         if LeftStr(LLinha,1) = '"' then
            Delete( LLinha, 1, 1);

         if RightStr(LLinha,1) = '"' then
            Delete( LLinha, Length(LLinha), 1);

         LSL2[J] := LLinha;
      end ;

      if (LSL2.Count >= 9) and (Length( OnlyNumber( LSL2[8] ) ) = 8) then
      begin
        LACBrCEPEnderecos := fOwner.Enderecos.New;

        LACBrCEPEnderecos.CEP             := LSL2[8];
        LACBrCEPEnderecos.Tipo_Logradouro := LSL2[0];
        LACBrCEPEnderecos.Logradouro      := LSL2[2];
        LACBrCEPEnderecos.Bairro          := LSL2[3];
        LACBrCEPEnderecos.Municipio       := LSL2[4];
        LACBrCEPEnderecos.UF              := LSL2[5];
        LACBrCEPEnderecos.IBGE_Municipio  := LSL2[7];
      end;
    end;
  finally
    LSL1.free;
    LSL2.free;
  end;

  BuscaEfetuada;
end;

{ TACBrWSRepublicaVirtual http://www.republicavirtual.com.br/cep/ *************}

constructor TACBrWSRepublicaVirtual.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://cep.republicavirtual.com.br/' ;
end;

procedure TACBrWSRepublicaVirtual.BuscarPorCEP(const ACEP : String);
begin
  FCepBusca := ACep; // republica virtual nao devolve o cep na resposta

  fOwner.HTTPGet( fpURL + 'web_cep.php?cep='+OnlyNumber( ACEP )+'&formato=xml' ) ;
  ProcessaResposta ;
end;

procedure TACBrWSRepublicaVirtual.BuscarPorLogradouro(const AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
begin
  inherited;
end;

procedure TACBrWSRepublicaVirtual.ProcessaResposta;
var
  LBuffer : string ;
  LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  fOwner.fEnderecos.Clear;

  LBuffer := fOwner.RespHTTP.Text;
  if StrToIntDef(SeparaDados(LBuffer, 'resultado'), 0) > 0 then
  begin
    LACBrCEPEnderecos := fOwner.Enderecos.New;

    LACBrCEPEnderecos.CEP             := FCepBusca ; // republica virtual nao devolve o cep na resposta
    LACBrCEPEnderecos.Tipo_Logradouro := SeparaDados(LBuffer,'tipo_logradouro') ;
    LACBrCEPEnderecos.Logradouro      := SeparaDados(LBuffer,'logradouro') ;
    LACBrCEPEnderecos.Complemento     := SeparaDados(LBuffer,'complemento') ;
    LACBrCEPEnderecos.Bairro          := SeparaDados(LBuffer,'bairro') ;
    LACBrCEPEnderecos.Municipio       := SeparaDados(LBuffer,'cidade') ;
    LACBrCEPEnderecos.UF              := SeparaDados(LBuffer,'uf') ;
    LACBrCEPEnderecos.IBGE_Municipio  := '';
  end ;

  BuscaEfetuada;
end ;


{ TACBrWSBases4you http://www.base4you.com ************************************}

constructor TACBrWSBases4you.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);

  fOwner.ParseText := True;
  fpURL := 'http://www.bases4you.com/wscep.php';
end;

procedure TACBrWSBases4you.BuscarPorCEP(const ACEP: String);
var
  LAcao: AnsiString;
begin
  LAcao :=
   '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'+
   '<SOAP-ENV:Envelope '+
     'xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" '+
     'xmlns:xsd="http://www.w3.org/2001/XMLSchema" '+
     'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" '+
     'xmlns:tns="urn:cepwsdl" '+
     'xmlns:soap="http://schemas.xmlsoap.org/wsdl/soap/" '+
     'xmlns:wsdl="http://schemas.xmlsoap.org/wsdl/" '+
     'xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" >'+
     '<SOAP-ENV:Body>'+
       '<mns:ConsultaCEP '+
         'xmlns:mns="urn:cepwsdl" '+
         'SOAP-ENV:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">'+
         '<userkey xsi:type="xsd:string">' + Trim( fOwner.ChaveAcesso ) + '</userkey>'+
         '<cep xsi:type="xsd:string">' + OnlyNumber( ACEP ) + '</cep>'+
       '</mns:ConsultaCEP>'+
     '</SOAP-ENV:Body>'+
   '</SOAP-ENV:Envelope>';

  try
    fOwner.HTTPSend.Clear;
    WriteStrToStream( fOwner.HTTPSend.Document, LAcao);
    fOwner.HTTPSend.Headers.Add( 'SoapAction: "urn:cepwsdl#ConsultaCEP"' );
    fOwner.HTTPPost(fpURL);
  except
    on E: Exception do
    begin
      raise EACBrCEPException.Create( E.Message );
    end;
  end;
end;

procedure TACBrWSBases4you.BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,
  ALogradouro, AUF, ABairro: String);
begin
  inherited;
end;

{ TACBrWSRNSolucoes }
constructor TACBrWSRNSolucoes.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);

  fOwner.ParseText := False;
  fpURL := 'http://www.rnsolucoes.com/wsRN.php?type=xml';
end;

procedure TACBrWSRNSolucoes.BuscarPorCEP(const ACEP: String);
begin
  try
    fOwner.HTTPSend.Clear;
    fOwner.HTTPPost(fpURL +
                    '&userkey=' + Trim(fOwner.ChaveAcesso) +
                    '&method=RetornaInfoCEP' +
                    '&orig=ACBR' +
                    '&cep=' + OnlyNumber(ACEP));

    ProcessaResposta;
  except
    on E: Exception do
    begin
      raise EACBrCEPException.Create( E.Message );
    end;
  end;
end;

procedure TACBrWSRNSolucoes.BuscarPorLogradouro(const AMunicipio,
    ATipo_Logradouro, ALogradouro, AUF, ABairro : String);
begin
  try
    fOwner.HTTPSend.Clear;

    fOwner.HTTPPost(fpURL +
                    '&userkey=' + Trim(fOwner.ChaveAcesso) +
                    '&method=RetornaInfoEndereco' +
                    '&orig=ACBR' +
                    '&uf=' + AUF +
                    '&cidade=' + AMunicipio +
                    '&endereco=' + ALogradouro);

    ProcessaResposta;
  except
    on E: Exception do
    begin
      raise EACBrCEPException.Create( E.Message );
    end;
  end;
end;

procedure TACBrWSRNSolucoes.ProcessaResposta;
var
  LBuffer: string;
  LACBrCEPEnderecos : TACBrCEPEndereco;
  s: string;
  i: Integer;
  LSL: TStringList;
begin
  LSL := TStringList.Create;

  try
    LBuffer := fOwner.RespHTTP.Text;
    LBuffer := StringReplace(LBuffer, sLineBreak, '', [rfReplaceAll]);
    LBuffer := StringReplace(LBuffer, '<dados>', '', [rfReplaceAll]);
    LBuffer := StringReplace(LBuffer, '</dados>', '', [rfReplaceAll]);
    LBuffer := StringReplace(LBuffer, '</dado>', '</dado>' + sLineBreak, [rfReplaceAll]);

    LSL.Text := LBuffer;

    for i := 0 to Pred( LSL.Count ) do
    begin
      s := LSL.Strings[i];

      if SeparaDados(s, 'cep') <> '' then
      begin
        LACBrCEPEnderecos := fOwner.Enderecos.New;

        LACBrCEPEnderecos.CEP             := SeparaDados(LBuffer, 'cep');
        LACBrCEPEnderecos.Tipo_Logradouro := '';
        LACBrCEPEnderecos.Logradouro      := SeparaDados(LBuffer, 'endereco');
        LACBrCEPEnderecos.Complemento     := SeparaDados(LBuffer, 'complemento');
        LACBrCEPEnderecos.Bairro          := SeparaDados(LBuffer, 'bairro');
        LACBrCEPEnderecos.Municipio       := SeparaDados(LBuffer, 'cidade');
        LACBrCEPEnderecos.UF              := SeparaDados(LBuffer, 'uf');
        LACBrCEPEnderecos.IBGE_Municipio  := '';
      end;
    end;
  finally
    LSL.Free;
    BuscaEfetuada;
  end;
end;


{ TACBrWSkingHost http://webservice.kinghost.net/*************}

constructor TACBrWSKingHost.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://webservice.kinghost.net/';
end;

procedure TACBrWSKingHost.BuscarPorCEP(const ACEP: String);
begin
  FCepBusca := ACep;

  fOwner.HTTPGet( fpURL + 'web_cep.php?'+
                          'auth='+ Trim(fOwner.ChaveAcesso) +
                          '&formato=xml'+
                          '&cep='+ OnlyNumber( ACEP ) ) ;
  ProcessaResposta ;
end;

procedure TACBrWSKingHost.BuscarPorLogradouro(const AMunicipio,
    ATipo_Logradouro, ALogradouro, AUF, ABairro : String);
begin
  inherited;
end;

procedure TACBrWSKingHost.ProcessaResposta;
var
  LBuffer    : string ;
  LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  fOwner.fEnderecos.Clear;

  LBuffer := fOwner.RespHTTP.Text;
  if StrToIntDef(SeparaDados(LBuffer, 'resultado'), 0) > 0 then
  begin
    LACBrCEPEnderecos := fOwner.Enderecos.New;
    LACBrCEPEnderecos.CEP             := FCepBusca ; // kingHost nao devolve o cep na resposta
    LACBrCEPEnderecos.Tipo_Logradouro := SeparaDados(LBuffer,'tipo_logradouro') ;
    LACBrCEPEnderecos.Logradouro      := SeparaDados(LBuffer,'logradouro') ;
    LACBrCEPEnderecos.Complemento     := SeparaDados(LBuffer,'complemento') ;
    LACBrCEPEnderecos.Bairro          := SeparaDados(LBuffer,'bairro') ;
    LACBrCEPEnderecos.Municipio       := SeparaDados(LBuffer,'cidade') ;
    LACBrCEPEnderecos.UF              := SeparaDados(LBuffer,'uf') ;
    LACBrCEPEnderecos.IBGE_Municipio  := '';
  end ;

  if Assigned( fOwner.OnBuscaEfetuada ) then
    fOwner.OnBuscaEfetuada( Self );
end ;

{ TACBrWSByJG  http://www.byjg.com.br/ ************************************}

constructor TACBrWSByJG.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://www.byjg.com.br/site/webservice.php/ws/cep' ;
end;

procedure TACBrWSByJG.BuscarPorCEP(const ACEP: String);
var
  LCEP: string;
begin
  TestarUsuario;
  FCepBusca  := ACEP;
  LCEP        := OnlyNumber( ACEP );
  FTipoBusca := 1;

  fOwner.HTTPGet( fpURL + '?httpmethod=obterlogradouroauth&cep='+LCEP+
                  '&usuario='+Trim(fOwner.Usuario)+'&senha='+Trim(fOwner.Senha)) ;

  ProcessaResposta ;
end;

procedure TACBrWSByJG.BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,
    ALogradouro, AUF, ABairro : String);
var
  LEndereco, LMunicipio, LUF : String;
begin
  TestarUsuario;

  LMunicipio       := fOwner.AjustaParam( AMunicipio ) ;
  LEndereco        := fOwner.AjustaParam(ATipo_Logradouro+' '+ALogradouro);
  LUF              := fOwner.AjustaParam( AUF );

  if (LMunicipio = '') or (LEndereco = '') or (LUF = '') then
     raise EACBrCEPException.Create( ERROR_LOGRADOURO_CIDADE_UF_OBRIGATORIO );

  FTipoBusca := 2;

  fOwner.HTTPGet( fpURL+'?httpmethod=obterCEPAuth&logradouro='+LEndereco+
                  '&localidade='+LMunicipio+
                  '&UF='+LUF+
                  '&usuario='+Trim(fOwner.Usuario)+'&senha='+Trim(fOwner.Senha)) ;
  ProcessaResposta ;
end;

procedure TACBrWSByJG.ProcessaResposta;
begin
  if FTipoBusca = 1 then
    ProcessaCEP
  else if FTipoBusca = 2 then
    ProcessaLogradouro;
end;

procedure TACBrWSByJG.ProcessaCEP;
var
  LBuffer, LResp, LTLog: TStringList;
  LTipoLogradouro, LLogra, LComp: string;
  LACBrCEPEnderecos : TACBrCEPEndereco;
  i, k : Integer;
begin
  LBuffer := TStringList.Create;
  try
    ExtractStrings(['|'],[], PChar(fOwner.RespHTTP.Text), LBuffer);

    i := CompareText(LBuffer[1], ACBrStr('Cep '+FCepBusca+' não encontrado'));
    k := CompareText(LBuffer[1], ACBrStr('CEP não está no formato 00000-000 ou 00000000'));

    if (i <> 0) and (k <> 0) then
    begin
      LResp := TStringList.Create;
      LTLog := TStringList.Create;
      try
        LLogra := '';
        LComp := '';
        LBuffer[1] := StringReplace(LBuffer[1], '''', '', [rfReplaceAll]);
        ExtractStrings([','],[], PChar(LBuffer[1]), LResp);
        ExtractStrings([' '],[], PChar(LResp[0]), LTLog);
        LTipoLogradouro := Trim(LTLog[0]);
        LTLog.Clear;
        ExtractStrings(['-'],[], PChar(LResp[0]), LTLog);
        LLogra := Trim(LTLog[0]);

        if(LTLog.Count > 1) then
          LComp := Trim(LTLog[1]);

        Delete(LLogra, 1, Length(LTipoLogradouro));

        LACBrCEPEnderecos := fOwner.Enderecos.New;

        LACBrCEPEnderecos.CEP             := Trim(FCepBusca);
        LACBrCEPEnderecos.Tipo_Logradouro := Trim(LTipoLogradouro);
        LACBrCEPEnderecos.Logradouro      := Trim(LLogra);
        LACBrCEPEnderecos.Complemento     := Trim(LComp);
        LACBrCEPEnderecos.Bairro          := Trim(LResp[1]);
        LACBrCEPEnderecos.Municipio       := Trim(LResp[2]);
        LACBrCEPEnderecos.UF              := Trim(LResp[3]);
        LACBrCEPEnderecos.IBGE_Municipio  := Trim(LResp[4]);
      finally
        LResp.Free;
        LTLog.Free;
      end;
    end ;
  finally
    if Assigned(LBuffer) then
      LBuffer.Free;

    BuscaEfetuada;
  end;
end ;

procedure TACBrWSByJG.ProcessaLogradouro;
var
   LBuffer, LResp, LTLog: TStringList;
   Qtd, i, k : Integer;
   LTipoLogradouro, LLogra, LComp: string;
   LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  LBuffer := TStringList.Create;
  try
    ExtractStrings(['|'],[], PChar(fOwner.RespHTTP.Text), LBuffer);

    if CompareText(LBuffer[2], ACBrStr('Logradouro não encontrado')) <> 0 then
    begin
      Qtd := StrToInt(LBuffer[1]);
      k := 2;

      LResp := TStringList.Create;
      LTLog := TStringList.Create;

      try
        for i := 1 to Qtd do
        begin
          LLogra := '';
          LComp  := '';

          ExtractStrings([','],[], PChar(LBuffer[k]), LResp);

          if CompareText(LResp[0], ACBrStr('00000000')) = 0 then
            Break;

          ExtractStrings([' '],[], PChar(LResp[1]), LTLog);
          LTipoLogradouro := Trim(LTLog[0]);
          LTLog.Clear;
          ExtractStrings(['-'],[], PChar(LResp[1]), LTLog);
          LLogra := Trim(LTLog[0]);

          if(LTLog.Count > 1) then
            LComp := Trim(LTLog[1]);

          Delete(LLogra, 1, Length(LTipoLogradouro));

          LACBrCEPEnderecos := fOwner.Enderecos.New;

          LACBrCEPEnderecos.CEP             := Trim(LResp[0]);
          LACBrCEPEnderecos.Tipo_Logradouro := Trim(LTipoLogradouro);
          LACBrCEPEnderecos.Logradouro      := Trim(LLogra);
          LACBrCEPEnderecos.Complemento     := Trim(LComp);
          LACBrCEPEnderecos.Bairro          := Trim(LResp[2]);
          LACBrCEPEnderecos.Municipio       := Trim(LResp[3]);
          LACBrCEPEnderecos.UF              := Trim(LResp[4]);
          LACBrCEPEnderecos.IBGE_Municipio  := Trim(LResp[5]);

          LResp.Clear;
          LTLog.Clear;

          Inc(k);
        end;
      finally
        LResp.Free;
        LTLog.Free;
      end;
    end;
  finally
    LBuffer.Free;
    BuscaEfetuada;
  end;
end;

{ TACBrWSDevMedia http://www.devmedia.com.br/devware/cep/service/ *************}

constructor TACBrWSDevMedia.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://www.devmedia.com.br/api/cep/service/';
end;

procedure TACBrWSDevMedia.BuscarPorCEP(const ACEP: String);
begin
  TestarChave;

  FCepBusca := ACep;

  fOwner.HTTPGet( fpURL + '?cep=' + OnlyNumber( ACEP ) + '&chave=' + Trim(fOwner.ChaveAcesso) + '&formato=xml' ) ;
  ProcessaResposta;
end;

procedure TACBrWSDevMedia.BuscarPorLogradouro(const AMunicipio,
    ATipo_Logradouro, ALogradouro, AUF, ABairro : String);
begin
  inherited;
end;

procedure TACBrWSDevMedia.ProcessaResposta;
var
  LResultado, LBuffer : String ;
  LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  fOwner.fEnderecos.Clear;

  LBuffer    := fOwner.RespHTTP.Text;
  LResultado := SeparaDados(LBuffer, 'resultado_txt') ;

  if LResultado = 'sucesso' then
  begin
    LACBrCEPEnderecos := fOwner.Enderecos.New;

    LACBrCEPEnderecos.CEP             := FCepBusca ; // DEVMEDIA nao devolve o cep na resposta
    LACBrCEPEnderecos.Tipo_Logradouro := SeparaDados(LBuffer,'tipo_logradouro') ;
    LACBrCEPEnderecos.Logradouro      := SeparaDados(LBuffer,'logradouro') ;
    LACBrCEPEnderecos.Complemento     := SeparaDados(LBuffer,'complemento') ;
    LACBrCEPEnderecos.Bairro          := SeparaDados(LBuffer,'bairro') ;
    LACBrCEPEnderecos.Municipio       := SeparaDados(LBuffer,'cidade') ;
    LACBrCEPEnderecos.UF              := SeparaDados(LBuffer,'uf') ;
    LACBrCEPEnderecos.IBGE_Municipio  := SeparaDados(LBuffer,'codigomunicipio');

  end
  else
     raise EACBrCEPException.Create( ACBrStr(LResultado) );

  BuscaEfetuada;
end ;


{ TACBrWSViaCEP }

constructor TACBrWSViaCEP.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fOwner.ParseText := True;
  fpURL := 'http://viacep.com.br/ws/';
end;

procedure TACBrWSViaCEP.BuscarPorCEP(const ACEP: String);
var
  LCEP: string;
begin
  FCepBusca := ACep;
  LCEP := OnlyNumber( ACEP );

  if ACEP = '' then
     raise EACBrCEPException.Create( ERROR_CEP_OBRIGATORIO );

  fOwner.HTTPGet( fpURL + LCEP + '/xml' ) ;
  ProcessaResposta();
end;

procedure TACBrWSViaCEP.BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,
    ALogradouro, AUF, ABairro : String);
var
  LMunicipio, LLogradouro, LUF, LURL: string;
begin
  if AMunicipio = '' then
     raise EACBrCEPException.Create( ERROR_MUNICIPIO_OBRIGATORIO );

  if ALogradouro = '' then
     raise EACBrCEPException.Create( ERROR_LOGRADOURO_OBRIGATORIO );

  if AUF = '' then
     raise EACBrCEPException.Create( ERROR_UF_OBRIGATORIO );

  LUF         := LowerCase(TiraAcentos(AUF));
  LMunicipio  := LowerCase(TiraAcentos(AMunicipio));
  LLogradouro := LowerCase(TiraAcentos(ALogradouro));
  LURL := EncodeURL(fpURL + LUF + '/' + LMunicipio + '/' + LLogradouro + '/xml');

  fOwner.HTTPGet(LURL);
  ProcessaResposta;
end;

procedure TACBrWSViaCEP.ProcessaResposta;
var
  LBuffer: string;
  LSL: TStringList;
  LACBrCEPEnderecos : TACBrCEPEndereco;
  s: string;
  i: Integer;
begin
  LSL := TStringList.Create;

  try
    LBuffer := UTF8ToNativeString(fOwner.RespHTTP.Text);
    LBuffer := StringReplace(LBuffer, sLineBreak, '', [rfReplaceAll]);
    LBuffer := StringReplace(LBuffer, '<enderecos>', '', [rfReplaceAll]);
    LBuffer := StringReplace(LBuffer, '</enderecos>', '', [rfReplaceAll]);
    LBuffer := StringReplace(LBuffer, '</endereco>', '</endereco>' + sLineBreak, [rfReplaceAll]);

    LSL.Text := LBuffer;

    for i := 0 to Pred( LSL.Count ) do
    begin
      s := LSL.Strings[i];

      if SeparaDados(s, 'cep') <> '' then
      begin
        LACBrCEPEnderecos := fOwner.Enderecos.New;

        LACBrCEPEnderecos.CEP             := SeparaDados(s, 'cep');
        LACBrCEPEnderecos.Tipo_Logradouro := '';
        LACBrCEPEnderecos.Logradouro      := SeparaDados(s, 'logradouro');
        LACBrCEPEnderecos.Complemento     := SeparaDados(s, 'complemento');
        LACBrCEPEnderecos.Bairro          := SeparaDados(s, 'bairro');
        LACBrCEPEnderecos.Municipio       := SeparaDados(s, 'localidade');
        LACBrCEPEnderecos.UF              := SeparaDados(s, 'uf');
        LACBrCEPEnderecos.IBGE_Municipio  := SeparaDados(s, 'ibge');
      end;
    end;
  finally
    LSL.Free;
    BuscaEfetuada;
  end;
end;



{ TACBrWSCorreiosSIGEP }
constructor TACBrWSCorreiosSIGEP.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fACBrIBGE := TACBrIBGE.Create(nil);

  fOwner.ParseText := false;
  fpURL := 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl';
end;

destructor TACBrWSCorreiosSIGEP.Destroy;
begin
  fACBrIBGE.Free;
  inherited Destroy;
end;

procedure TACBrWSCorreiosSIGEP.BuscarPorCEP(const ACEP: String);
var
  LAcao: AnsiString;
begin
  LAcao :=
     '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'
       + '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" '
       + 'xmlns:cli="http://cliente.bean.master.sigep.bsb.correios.com.br/"> '
       + ' <soapenv:Header/>'
       + ' <soapenv:Body>'
       + ' <cli:consultaCEP>'
       + ' <cep>' + ACEP + '</cep>'
       + ' </cli:consultaCEP>'
       + ' </soapenv:Body>'
       + ' </soapenv:Envelope>';

  try
    fOwner.HTTPSend.Clear;
    WriteStrToStream(fOwner.HTTPSend.Document, LAcao);
    fOwner.HTTPPost(fpURL);

    ProcessaResposta;
  except
    on E: Exception do
    begin
      if Pos('CEP NAO ENCONTRADO', E.Message) <> 0  then
        raise EACBrCEPException.Create( ERROR_CEP_NAO_ENCONTRADO )
      else
        raise EACBrCEPException.Create( 'Ocorreu o seguinte erro ao consumir o WebService dos correios:' + sLineBreak + '  - ' + E.Message );
    end;
  end;
end;

procedure TACBrWSCorreiosSIGEP.ProcessaResposta;
var
  LBuffer: string;
  LACBrCEPEnderecos : TACBrCEPEndereco;
  s: string;
  i: Integer;
  LSL: TStringList;
  LMuninicipioEncontrado, LComplemento2: String;
begin
  LSL := TStringList.Create;

  fACBrIBGE.ProxyHost := fOwner.ProxyHost;
  fACBrIBGE.ProxyPort := fOwner.ProxyPort;
  fACBrIBGE.ProxyPass := fOwner.ProxyPass;
  fACBrIBGE.ProxyUser := fOwner.ProxyUser;

  try
    LBuffer := fOwner.RespHTTP.Text;
    LBuffer := StringReplace(LBuffer, sLineBreak, '', [rfReplaceAll]);

    LSL.Text := LBuffer;
    LMuninicipioEncontrado := '';

    for i := 0 to Pred( LSL.Count ) do
    begin
      s := LSL.Strings[i];

      if SeparaDados(s, 'cep') <> '' then
      begin
        LACBrCEPEnderecos := fOwner.Enderecos.New;

        LACBrCEPEnderecos.CEP             := SeparaDados(LBuffer, 'cep');
        LACBrCEPEnderecos.Tipo_Logradouro := '';
        LACBrCEPEnderecos.Logradouro      := SeparaDados(LBuffer, 'end');
        LACBrCEPEnderecos.Complemento     := Trim(SeparaDados(LBuffer, 'complemento'));
        LComplemento2              := Trim(SeparaDados(LBuffer, 'complemento2'));
        LACBrCEPEnderecos.Bairro          := SeparaDados(LBuffer, 'bairro');
        LACBrCEPEnderecos.Municipio       := SeparaDados(LBuffer, 'cidade');
        LACBrCEPEnderecos.UF              := SeparaDados(LBuffer, 'uf');
        LACBrCEPEnderecos.IBGE_Municipio  := '';

        if LComplemento2 <> '' then
        begin
          if LACBrCEPEnderecos.Complemento <> ''  then
            LACBrCEPEnderecos.Complemento := LACBrCEPEnderecos.Complemento + ' ';

          LACBrCEPEnderecos.Complemento := LACBrCEPEnderecos.Complemento + LComplemento2;
        end;

        // Correios não retornam informação do IBGE, Fazendo busca do IBGE com ACBrIBGE //
        if (LACBrCEPEnderecos.Municipio <> '') and (fOwner.PesquisarIBGE) then
        begin
          if (LMuninicipioEncontrado <> LACBrCEPEnderecos.Municipio) then  // Evita buscar municipio já encontrado
          begin
            fACBrIBGE.BuscarPorNome( LACBrCEPEnderecos.Municipio, LACBrCEPEnderecos.UF, True) ;
            LMuninicipioEncontrado := LACBrCEPEnderecos.Municipio;
          end ;

          if fACBrIBGE.Cidades.Count > 0 then  // Achou ?
             LACBrCEPEnderecos.IBGE_Municipio := IntToStr( fACBrIBGE.Cidades[0].CodMunicipio );
        end ;
      end;
    end;
  finally
    LSL.Free;
    BuscaEfetuada;
  end;
end;

procedure TACBrWSCEPAberto.BuscarPorLogradouro(const AMunicipio,
    ATipo_Logradouro, ALogradouro, AUF, ABairro : String);
var
  LParametros: string;
  LMunicipio, LLogradouro, LUF, LBairro : string;
begin
  TestarChave;

  if AUF = '' then
     raise EACBrCEPException.Create( ERROR_UF_OBRIGATORIO );

  if AMunicipio = '' then
     raise EACBrCEPException.Create( ERROR_MUNICIPIO_OBRIGATORIO );

  LUF         := fOwner.AjustaParam(AUF);
  LMunicipio  := fOwner.AjustaParam(AMunicipio);
  LLogradouro := fOwner.AjustaParam(ALogradouro);
  LBairro     := fOwner.AjustaParam(ABairro);

  // estado e cidade são obrigatórios
  LParametros := 'estado=' + LUF + '&cidade=' + LMunicipio;

  if(Trim(LBairro) <> '')then
    LParametros := LParametros + '&bairro=' + LBairro;

  if(Trim(LLogradouro) <> '')then
    LParametros := LParametros + '&logradouro=' + LLogradouro;

  fOwner.HTTPSend.Clear;
  fOwner.HTTPSend.Headers.Add('Authorization: Token token="'
                               + fOwner.ChaveAcesso
                               + '"');
  fOwner.HTTPMethod('GET', fpURL+'address?' + LParametros);

  ProcessaResposta;
end;

constructor TACBrWSCEPAberto.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);

  fOwner.ParseText := False;
  fpURL := 'http://www.cepaberto.com/api/v3/';
end;

procedure TACBrWSCEPAberto.BuscarPorCEP(const ACEP: String);
var
  LCEP: string;
begin
  LCEP := OnlyNumber( ACEP );

  if LCEP = '' then
    raise EACBrCEPException.Create( ERROR_CEP_OBRIGATORIO );

  TestarChave;

  fOwner.HTTPSend.Clear;
  fOwner.HTTPSend.Headers.Add('Authorization: Token token="'+ fOwner.ChaveAcesso +'"');
  fOwner.HTTPMethod('GET', fpURL+'cep?cep=' + LCEP);

  ProcessaResposta;
end;

procedure TACBrWSCEPAberto.ProcessaResposta;
var
  LJson      : TACBrJSONObject;
  LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  LJson := TACBrJSONObject.Parse(fOwner.RespHTTP.Text);

  if (LJson.AsString['cep'] <> '') then
  begin
    try
      LACBrCEPEnderecos := fOwner.Enderecos.New;

      LACBrCEPEnderecos.CEP             := LJson.AsString['cep'];
      LACBrCEPEnderecos.Bairro          := LJson.AsString['bairro'];
      LACBrCEPEnderecos.Logradouro      := LJson.AsString['logradouro'];
      LACBrCEPEnderecos.Municipio       := LJson.AsJSONObject['cidade'].AsString['nome'];
      LACBrCEPEnderecos.IBGE_Municipio  := LJson.AsJSONObject['cidade'].AsString['ibge'];
      LACBrCEPEnderecos.UF              := LJson.AsJSONObject['estado'].AsString['sigla'];
      LACBrCEPEnderecos.Latitude        := LJson.AsString['latitude'];
      LACBrCEPEnderecos.Longitude       := LJson.AsString['longitude'];
      LACBrCEPEnderecos.Altitude        := LJson.AsString['altitude'];
    finally
      LJson.Free;

      BuscaEfetuada;
    end;
  end
  else
  begin
    LJson.Free;
    BuscaEfetuada;
  end;
end;

{ TACBrWSWSCEP }

procedure TACBrWSWSCEP.BuscarPorCEP(const ACEP: String);
var
  LCEP: string;
begin
  LCEP := OnlyNumber( ACEP );

  if LCEP = '' then
    raise EACBrCEPException.Create( ERROR_CEP_OBRIGATORIO );

  TestarChave;

  fOwner.HTTPSend.Clear;
  fOwner.HTTPMethod('GET', Format(fpURL, [fOwner.ChaveAcesso, LCEP]));

  ProcessaResposta;
end;

constructor TACBrWSWSCEP.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fOwner.ParseText := False;
  fpURL := 'http://api.wscep.com/cep?key=%s&val=%s';
end;

procedure TACBrWSWSCEP.ProcessaResposta;
var
  LBuffer: string;
  LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  LBuffer := fOwner.RespHTTP.Text;

  LBuffer := StringReplace(LBuffer, '<?xml version="1.0" encoding="UTF-8"?>' +sLineBreak+ '<cep>', '<?xml version="1.0" encoding="UTF-8"?>' +sLineBreak+ '<resposta>', [rfReplaceAll]);
  LBuffer := StringReplace(LBuffer, '</estado>' +sLineBreak+ '</cep>', '</estado>' +sLineBreak+ '</resposta>', [rfReplaceAll]);
  LBuffer := StringReplace(LBuffer, sLineBreak, '', [rfReplaceAll]);

  if SeparaDados(LBuffer, 'cep') <> '' then
  begin
    LACBrCEPEnderecos := fOwner.Enderecos.New;

    LACBrCEPEnderecos.CEP             := SeparaDados(LBuffer, 'cep');
    LACBrCEPEnderecos.Logradouro      := SeparaDados(LBuffer, 'logradouro');
    LACBrCEPEnderecos.Bairro          := SeparaDados(LBuffer, 'bairro');
    LACBrCEPEnderecos.Municipio       := SeparaDados(LBuffer, 'cidade');
    LACBrCEPEnderecos.UF              := SeparaDados(LBuffer, 'uf');
    LACBrCEPEnderecos.IBGE_Municipio  := SeparaDados(LBuffer, 'cod_ibge_municipio');
    LACBrCEPEnderecos.Altitude        := SeparaDados(LBuffer, 'alt');
    LACBrCEPEnderecos.Latitude        := SeparaDados(LBuffer, 'lat');
    LACBrCEPEnderecos.Longitude       := SeparaDados(LBuffer, 'lng');
  end;

  BuscaEfetuada;
end;

{ TACBrWSOpenCEP }

procedure TACBrWSOpenCEP.BuscarPorCEP(const ACEP: String);
var
  LCEP: string;
begin
  FCepBusca := ACep;
  LCEP := OnlyNumber( ACEP );

  if ACEP = '' then
     raise EACBrCEPException.Create( ERROR_CEP_OBRIGATORIO );

  fOwner.HTTPGet( fpURL + LCEP ) ;
  ProcessaResposta();
end;

procedure TACBrWSOpenCEP.BuscarPorLogradouro(const AMunicipio, ATipo_Logradouro,
  ALogradouro, AUF, ABairro: String);
begin
  inherited;
end;

constructor TACBrWSOpenCEP.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fOwner.ParseText := True;
  fpURL := 'https://opencep.com/v1/';
end;

procedure TACBrWSOpenCEP.ProcessaResposta;
var
  LJson : TACBrJSONObject;
  LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  LJson := TACBrJSONObject.Parse( UTF8ToNativeString( fOwner.RespHTTP.Text ));

  try
    LACBrCEPEnderecos := fOwner.Enderecos.New;

    LACBrCEPEnderecos.CEP             := LJson.AsString['cep'];
    LACBrCEPEnderecos.Logradouro      := LJson.AsString['logradouro'];
    LACBrCEPEnderecos.Complemento     := LJson.AsString['complemento'];
    LACBrCEPEnderecos.Bairro          := LJson.AsString['bairro'];
    LACBrCEPEnderecos.Municipio       := LJson.AsString['localidade'];
    LACBrCEPEnderecos.UF              := LJson.AsString['uf'];
    LACBrCEPEnderecos.IBGE_Municipio  := LJson.AsString['ibge'];

  finally
    LJson.Free;
    BuscaEfetuada;
  end;
end;

{ TACBrWSBrasilAPI }

procedure TACBrWSBrasilAPI.BuscarPorCEP(const ACEP: String);
var
  LCEP: string;
begin
  FCepBusca := ACep;
  LCEP := OnlyNumber( ACEP );

  if ACEP = '' then
     raise EACBrCEPException.Create( ERROR_CEP_OBRIGATORIO );

  fOwner.HTTPGet( fpURL + LCEP ) ;
  ProcessaResposta();
end;

procedure TACBrWSBrasilAPI.BuscarPorLogradouro(const AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
begin
  inherited;
end;

constructor TACBrWSBrasilAPI.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fOwner.ParseText := True;
  fpURL := 'https://brasilapi.com.br/api/cep/v2/';
end;

procedure TACBrWSBrasilAPI.ProcessaResposta;
var
  LJson : TACBrJSONObject;
  LACBrCEPEnderecos : TACBrCEPEndereco;
begin
  LJson := TACBrJSONObject.Parse(fOwner.RespHTTP.Text);

  try
    LACBrCEPEnderecos := fOwner.Enderecos.New;

    LACBrCEPEnderecos.CEP             := LJson.AsString['cep'];
    LACBrCEPEnderecos.Logradouro      := LJson.AsString['street'];
    LACBrCEPEnderecos.Complemento     := LJson.AsString['complemento'];
    LACBrCEPEnderecos.Bairro          := LJson.AsString['neighborhood'];
    LACBrCEPEnderecos.Municipio       := LJson.AsString['city'];
    LACBrCEPEnderecos.UF              := LJson.AsString['state'];
    LACBrCEPEnderecos.Longitude       := LJson.AsJSONObject['location'].AsJSONObject['coordinates'].AsString['longitude'];
    LACBrCEPEnderecos.Latitude        := LJson.AsJSONObject['location'].AsJSONObject['coordinates'].AsString['latitude'];
  finally
    LJson.Free;
    BuscaEfetuada;
  end;
end;

end.
