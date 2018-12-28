{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 12/08/2010: Primeira Versao
|*    Daniel Simoes de Almeida e André Moraes
|*
|* 24/05/2018: Versão X
|*    Igor de Bastos Costa
|*    Atualizacao dos comandos do CEPAberto que alterou a API para versao 3.0
******************************************************************************}

unit ACBrCEP ;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils, contnrs, ACBrSocket, ACBrIBGE, Jsons;

type
  TACBrCEPWebService = ( wsNenhum, wsBuscarCep, wsCepLivre, wsRepublicaVirtual,
                         wsBases4you, wsRNSolucoes, wsKingHost, wsByJG,
                         wsCorreios, wsDevMedia, wsViaCep, wsCorreiosSIGEP,
                         wsCepAberto, wsWSCep) ;

  EACBrCEPException = class ( Exception );

  { TACBrCEPEndereco }

  TACBrCEPEndereco = class
    private
      fBairro : String ;
      fCEP : String ;
      fCodigoIBGE : String ;
      fComplemento : String ;
      fLogradouro : String ;
      fMunicipio : String ;
      fTipo_Logradouro : String ;
      fUF : String ;
      fAltitude: string;
      fLatitude: string;
      fLongitude: string;

      function GetIBGE_UF : String ;
    public
      constructor Create ;

      property CEP             : String read fCEP             write fCEP ;
      property Tipo_Logradouro : String read fTipo_Logradouro write fTipo_Logradouro ;
      property Logradouro      : String read fLogradouro      write fLogradouro ;
      property Complemento     : String read fComplemento     write fComplemento ;
      property Bairro          : String read fBairro          write fBairro ;
      property Municipio       : String read fMunicipio       write fMunicipio ;
      property UF              : String read fUF              write fUF ;
      property IBGE_Municipio  : String read fCodigoIBGE      write fCodigoIBGE ;
      property IBGE_UF         : String read GetIBGE_UF ;
      property Altitude        : String read fAltitude        write fAltitude ;
      property Latitude        : String read fLatitude        write fLatitude ;
      property Longitude       : String read fLongitude       write fLongitude ;
  end ;

  { Lista de Objetos do tipo TACBrCEPEndereco }

  { TACBrCEPEnderecos }

  TACBrCEPEnderecos = class(TObjectList)
    protected
      procedure SetObject (Index: Integer; Item: TACBrCEPEndereco);
      function GetObject (Index: Integer): TACBrCEPEndereco;
      procedure Insert (Index: Integer; Obj: TACBrCEPEndereco);
    public
      function Add (Obj: TACBrCEPEndereco): Integer;
      function New: TACBrCEPEndereco ;
      property Objects [Index: Integer]: TACBrCEPEndereco
        read GetObject write SetObject; default;
    end;

  TACBrCEPWSClass = class ;

  { TACBrCEP }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrCEP = class( TACBrHTTP )
    private
      fWebService : TACBrCEPWebService ;
      fACBrCEPWS  : TACBrCEPWSClass ;

      fEnderecos : TACBrCEPEnderecos ;
      fOnBuscaEfetuada : TNotifyEvent ;
      fChaveAcesso: String;
      fUsuario : String;
      fSenha : String;
      FPesquisarIBGE: Boolean;
      function GetURL : String ;
      procedure SetWebService(const AValue : TACBrCEPWebService) ;
    public
      constructor Create(AOwner: TComponent); override;
      Destructor Destroy ; override ;

      property Enderecos : TACBrCEPEnderecos  read fEnderecos ;

      function BuscarPorCEP( ACEP : String ) : Integer ;
      function BuscarPorLogradouro( ACidade, ATipo_Logradouro, ALogradouro, AUF,
         ABairro : String ) : Integer ;

    published
      property WebService : TACBrCEPWebService read fWebService write SetWebService default wsNenhum ;
      property URL : String read GetURL ;
      property ChaveAcesso: String read fChaveAcesso write fChaveAcesso ;
      property Usuario: String read fUsuario write fUsuario ;
      property Senha: String read fSenha write fSenha ;
      property PesquisarIBGE: Boolean read FPesquisarIBGE write FPesquisarIBGE; // Válido somente para wsCorreios e TACBrWSCorreiosSIGEP

      property OnBuscaEfetuada : TNotifyEvent read fOnBuscaEfetuada
         write fOnBuscaEfetuada ;
  end ;

  { TACBrCEPWSClass }
	{$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrCEPWSClass = class
    private
      fOwner : TACBrCEP ;
      fpURL : String ;

      procedure ErrorAbstract ;
    protected
      procedure TestarChave;
      procedure TestarUsuario;
    public
      constructor Create( AOwner : TACBrCEP ) ; virtual ;

      Procedure BuscarPorCEP( ACEP : String ) ; virtual ;
      Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
         AUF, ABairro : String ) ; virtual ;

      property URL : String read fpURL ;
  end ;

  { TACBrWSBuscarCEP }

  TACBrWSBuscarCEP = class(TACBrCEPWSClass)
    private
      procedure ProcessaResposta ;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP( ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
         AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSCEPLivre }

  TACBrWSCEPLivre = class(TACBrCEPWSClass)
    private
      procedure ProcessaResposta ;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP( ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
         AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSRepublicaVirtual }

  TACBrWSRepublicaVirtual = class(TACBrCEPWSClass)
    private
      FCepBusca: String;
      procedure ProcessaResposta ;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP( ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
         AUF, ABairro : String ) ; override ;
  end ;

  TACBrWSBases4you = class(TACBrCEPWSClass)
    private
      procedure ProcessaResposta ;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP( ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
         AUF, ABairro : String ) ; override ;
  end ;

  TACBrWSRNSolucoes = class(TACBrCEPWSClass)
  private
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;
    Procedure BuscarPorCEP( ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
       AUF, ABairro : String ) ; override ;
  end;

  TACBrWSKingHost = class(TACBrCEPWSClass)
  private
    FCepBusca: String;
    procedure ProcessaResposta ;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;

    Procedure BuscarPorCEP( ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
       AUF, ABairro : String ) ; override ;
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

    Procedure BuscarPorCEP( ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
       AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSCorreios }

  TACBrWSCorreios = class(TACBrCEPWSClass)
  private
    fACBrIBGE: TACBrIBGE;
    procedure ProcessaResposta;
  public
    constructor Create( AOwner: TACBrCEP ); override ;
    destructor Destroy; override;

    procedure BuscarPorCEP( ACEP: String ); override ;
    procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
         AUF, ABairro : String ) ; override ;
  end;

{ TACBrDevMedia }

TACBrWSDevMedia = class(TACBrCEPWSClass)
    private
      FCepBusca: String;
      procedure TestarChave;
      procedure ProcessaResposta;
    public
      constructor Create( AOwner : TACBrCEP ) ; override ;

      Procedure BuscarPorCEP( ACEP : String ) ; override ;
      Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro,
          ALogradouro, AUF, ABairro : String ) ; override ;
  end ;

  { TACBrWSViaCEP }
  TACBrWSViaCEP = class(TACBrCEPWSClass)
  private
    FCepBusca: String;
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ); override;

    Procedure BuscarPorCEP( ACEP : String ); override;
    Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro,ALogradouro, AUF, ABairro : String ); override;
  end;

  { TACBrWSCorreiosSIGEP }

  TACBrWSCorreiosSIGEP = class(TACBrCEPWSClass)
  private
    fACBrIBGE: TACBrIBGE;
    procedure ProcessaResposta;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;
    destructor Destroy; override;
    Procedure BuscarPorCEP( ACEP : String ) ; override ;
  end;

  { TACBrWSCEPAberto }

  TACBrWSCEPAberto= class(TACBrCEPWSClass)
  private
    procedure ProcessaResposta ;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;
    Procedure BuscarPorCEP( ACEP : String ) ; override ;
    Procedure BuscarPorLogradouro( AMunicipio, ATipo_Logradouro,ALogradouro, AUF, ABairro : String ); override;
  end ;

  { TACBrWSWSCEP } // WSCep é o nome do servico

  TACBrWSWSCEP = class(TACBrCEPWSClass)
  private
    procedure ProcessaResposta ;
  public
    constructor Create( AOwner : TACBrCEP ) ; override ;
    Procedure BuscarPorCEP( ACEP : String ) ; override ;
  end ;

implementation

uses strutils, math, synacode, ACBrUtil ;

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
  inherited SetItem (Index, Item) ;
end ;

function TACBrCEPEnderecos.GetObject(Index : Integer) : TACBrCEPEndereco ;
begin
  Result := inherited GetItem(Index) as TACBrCEPEndereco ;
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

  fEnderecos  := TACBrCEPEnderecos.create( True );
  fACBrCEPWS  := TACBrCEPWSClass.Create( Self );
  fWebService := wsNenhum ;
  FPesquisarIBGE := True;
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
    //wsCorreios       : fACBrCEPWS := TACBrWSCorreios.Create(Self);      // WebService antigo do correios morreu :(
    wsCorreios         : fACBrCEPWS := TACBrWSCorreiosSIGEP.Create(Self);
    wsDevMedia         : fACBrCEPWS := TACBrWSDevMedia.Create(Self);
    wsViaCep           : fACBrCEPWS := TACBrWSViaCEP.Create(Self);
    wsCorreiosSIGEP    : fACBrCEPWS := TACBrWSCorreiosSIGEP.Create(Self);
    wsCepAberto        : fACBrCEPWS := TACBrWSCEPAberto.Create(Self);
    wsWSCep            : fACBrCEPWS := TACBrWSWSCEP.Create(Self);
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
     raise EACBrCEPException.Create('CEP deve ser informado');

  fACBrCEPWS.BuscarPorCEP(ACEP);

  Result := fEnderecos.Count;
end ;

function TACBrCEP.BuscarPorLogradouro(ACidade, ATipo_Logradouro, ALogradouro,
  AUF, ABairro : String ) : Integer ;
begin
  fEnderecos.Clear;
  fACBrCEPWS.BuscarPorLogradouro( ACidade, ATipo_Logradouro, ALogradouro, AUF,
                                  ABairro );

  Result := fEnderecos.Count;
end ;

{ TACBrCEPWSClass *************************************************************}

procedure TACBrCEPWSClass.ErrorAbstract ;
begin
  raise EACBrCEPException.Create( 'Nenhum WebService selecionado' )
end ;

procedure TACBrCEPWSClass.TestarChave;
begin
  if fOwner.ChaveAcesso = EmptyStr then
    raise EACBrCEPException.Create( ACBrStr('Chave de acesso não informada.') );
end;

procedure TACBrCEPWSClass.TestarUsuario;
begin
  if fOwner.Usuario = EmptyStr then
    raise EACBrCEPException.Create( ACBrStr('Usuario não informado.') )
  else if fOwner.Senha = EmptyStr then
    raise EACBrCEPException.Create( ACBrStr('Senha não informada.') );
end;

constructor TACBrCEPWSClass.Create( AOwner : TACBrCEP) ;
begin
  inherited Create ;
  fOwner := AOwner;
  fpURL  := '';
end ;

Procedure TACBrCEPWSClass.BuscarPorCEP(ACEP : String) ;
begin
  ErrorAbstract ;
end ;

Procedure TACBrCEPWSClass.BuscarPorLogradouro(AMunicipio, ATipo_Logradouro,
   ALogradouro, AUF, ABairro : String ) ;
begin
  ErrorAbstract ;
end ;

{ TACBrWSBuscarCEP - http://www.buscarcep.com.br *******************************}

constructor TACBrWSBuscarCEP.Create(AOwner : TACBrCEP) ;
begin
  inherited Create(AOwner) ;

  fpURL := 'http://www.buscarcep.com.br/' ;
end ;

Procedure TACBrWSBuscarCEP.BuscarPorCEP( ACEP : String ) ;
begin
  TestarChave;

  fOwner.HTTPGet( fpURL + '?cep='+ACEP+'&formato=string&chave='+fOwner.ChaveAcesso ) ;
  ProcessaResposta ;
end ;

Procedure  TACBrWSBuscarCEP.BuscarPorLogradouro(AMunicipio,  ATipo_Logradouro,
  ALogradouro, AUF, ABairro : String) ;
Var
   Params : String ;
begin
  TestarChave;

  AMunicipio       := fOwner.AjustaParam( AMunicipio ) ;
  ATipo_Logradouro := fOwner.AjustaParam( ATipo_Logradouro );
  ALogradouro      := fOwner.AjustaParam( ALogradouro ) ;
  AUF              := fOwner.AjustaParam( AUF );

  if (AMunicipio = '') or (ALogradouro = '') or (AUF = '') then
     raise EACBrCEPException.Create('UF, Cidade e Logradouro devem ser informados');

  Params := '?logradouro=' + ALogradouro+
            '&cidade='     + AMunicipio+
            '&uf='         + AUF ;

  if ATipo_Logradouro <> '' then
    Params := Params + '&tipo_logradouro=' + ATipo_Logradouro ;

  if ABairro <> '' then
    Params := Params + '&bairro=' + ABairro ;

  Params := Params + '&formato=string' ;
  Params := Params + '&chave=' + fOwner.ChaveAcesso;

  fOwner.HTTPGet( fpURL + Params ) ;
  ProcessaResposta ;
end ;

Procedure TACBrWSBuscarCEP.ProcessaResposta ;
Var
   SL1, SL2 : TStringList ;
   Buffer : String ;
   PosIni, I : Integer ;
begin
  fOwner.fEnderecos.Clear;

  SL1 := TStringList.Create;
  SL2 := TStringList.Create;
  try
    SL1.Text := StringReplace( fOwner.RespHTTP.Text, '&cep=', sLineBreak+'&cep=',
                               [rfReplaceAll] );

    For I := 0 to SL1.Count-1 do
    begin
       Buffer := SL1[I] ;
       PosIni := pos('&cep=',Buffer) ;

       if PosIni > 0 then
       begin

         Buffer := copy( Buffer, PosIni, Length(Buffer) ) ;

         SL2.Clear;
         SL2.Text := StringReplace( Buffer, '&', sLineBreak, [rfReplaceAll] );

         if (SL2.Values['resultado'] = '1') then
         begin
            with fOwner.Enderecos.New do
            begin
              CEP             := SL2.Values['cep'] ;
              Tipo_Logradouro := SL2.Values['tipo_logradouro'] ;
              Logradouro      := SL2.Values['logradouro'] ;
              Complemento     := SL2.Values['complemento'] ;
              Bairro          := SL2.Values['bairro'] ;
              Municipio       := SL2.Values['cidade'] ;
              UF              := SL2.Values['uf'] ;
              IBGE_Municipio  := SL2.Values['ibge_municipio_verificador'] ;
            end ;
         end ;
       end ;
    end ;
  finally
    SL1.free ;
    SL2.free ;
  end ;

  if Assigned( fOwner.OnBuscaEfetuada ) then
     fOwner.OnBuscaEfetuada( Self );
end ;


{ TACBrWSCEPLivre - http://ceplivre.com.br/ ***********************************}

constructor TACBrWSCEPLivre.Create(AOwner : TACBrCEP) ;
begin
  inherited Create(AOwner) ;
  fpURL := 'http://ceplivre.com.br/consultar/' ;
end ;

Procedure TACBrWSCEPLivre.BuscarPorCEP(ACEP : String) ;
begin
  TestarChave;

  // CEPLivre exige CEP formatado //
  ACEP := Copy(ACEP,1,5)+'-'+Copy(ACEP,6,3);

  fOwner.HTTPGet( fpURL + 'cep/' + Trim(fOwner.ChaveAcesso) + '/' + ACEP + '/csv') ;
  ProcessaResposta ;
end ;

Procedure TACBrWSCEPLivre.BuscarPorLogradouro(AMunicipio, ATipo_Logradouro,
  ALogradouro, AUF, ABairro : String) ;
begin
  TestarChave;

  ALogradouro := fOwner.AjustaParam( ALogradouro ) ;
  if (ALogradouro = '') then
     raise EACBrCEPException.Create('Cidade e Logradouro devem ser informados');

  fOwner.HTTPGet( fpURL + 'logradouro/' + Trim( fOwner.ChaveAcesso ) + '/' + Alogradouro + '/csv' ) ;
  ProcessaResposta ;
end ;

Procedure TACBrWSCEPLivre.ProcessaResposta ;
Var
   SL1, SL2 : TStringList ;
   Buffer, Linha : String ;
   I, J : Integer ;
begin
  fOwner.fEnderecos.Clear;

  SL1 := TStringList.Create;
  SL2 := TStringList.Create;
  try
    Buffer := fOwner.RespHTTP.Text ;
    // CEP livre retorna vários endereços na mesma linha... tratando...
    SL1.Text := StringReplace( Buffer, '""', '"'+sLineBreak+'"', [rfReplaceAll] );

    For I := 0 to SL1.Count-1 do
    begin
      Buffer := SL1[I] ;

      SL2.Clear;
      SL2.Text := StringReplace( Buffer, ',', sLineBreak, [rfReplaceAll] );

      { Removendo as aspas do inicio e fim }
      for J := 0 to SL2.Count-1 do
      begin
         Linha := SL2[J] ;
         if LeftStr(Linha,1) = '"' then
            Delete( Linha, 1, 1) ;
         if RightStr(Linha,1) = '"' then
            Delete( Linha, Length(Linha), 1) ;

         SL2[J] := Linha;
      end ;

      if (SL2.Count >= 9) and (Length( OnlyNumber( SL2[8] ) ) = 8) then
      begin
        with fOwner.Enderecos.New do
        begin
          CEP             := SL2[8] ;
          Tipo_Logradouro := SL2[0] ;
          Logradouro      := SL2[2] ;
          Bairro          := SL2[3] ;
          Municipio       := SL2[4] ;
          UF              := SL2[5] ;
          IBGE_Municipio  := SL2[7] ;
        end ;
      end ;
    end ;
  finally
    SL1.free ;
    SL2.free ;
  end ;

  if Assigned( fOwner.OnBuscaEfetuada ) then
     fOwner.OnBuscaEfetuada( Self );
end ;

{ TACBrWSRepublicaVirtual http://www.republicavirtual.com.br/cep/ *************}

constructor TACBrWSRepublicaVirtual.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://cep.republicavirtual.com.br/' ;
end;

procedure TACBrWSRepublicaVirtual.BuscarPorCEP(ACEP: String);
begin
  FCepBusca := ACep; // republica virtual nao devolve o cep na resposta
  ACEP := OnlyNumber( ACEP );

  fOwner.HTTPGet( fpURL + 'web_cep.php?cep='+ACEP+'&formato=xml' ) ;
  ProcessaResposta ;
end;

procedure TACBrWSRepublicaVirtual.BuscarPorLogradouro(AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
begin
  raise EACBrCEPException.Create(ACBrStr('Busca por Logradouro não disponível no site Republica Virtual.'));
end;

procedure TACBrWSRepublicaVirtual.ProcessaResposta;
var
  Buffer : String ;
begin
  fOwner.fEnderecos.Clear;

  Buffer := fOwner.RespHTTP.Text;
  if StrToIntDef(LerTagXML(Buffer, 'resultado'), 0) > 0 then
  begin
    with fOwner.Enderecos.New do
    begin
      CEP             := FCepBusca ; // republica virtual nao devolve o cep na resposta
      Tipo_Logradouro := LerTagXML(Buffer,'tipo_logradouro') ;
      Logradouro      := LerTagXML(Buffer,'logradouro') ;
      Complemento     := LerTagXML(Buffer,'complemento') ;
      Bairro          := LerTagXML(Buffer,'bairro') ;
      Municipio       := LerTagXML(Buffer,'cidade') ;
      UF              := LerTagXML(Buffer,'uf') ;
      IBGE_Municipio  := '';
    end ;
  end ;

  if Assigned( fOwner.OnBuscaEfetuada ) then
    fOwner.OnBuscaEfetuada( Self );
end ;


{ TACBrWSBases4you http://www.base4you.com ************************************}

constructor TACBrWSBases4you.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);

  fOwner.ParseText := True;
  fpURL := 'http://www.bases4you.com/wscep.php';
end;

procedure TACBrWSBases4you.BuscarPorCEP(ACEP: String);
var
  Acao: TStringList;
  Stream: TMemoryStream;
begin
  Acao   := TStringList.Create;
  Stream := TMemoryStream.Create;
  try
    Acao.Text :=
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
      Acao.SaveToStream(Stream);

      fOwner.HTTPSend.Clear;
      fOwner.HTTPSend.Document.LoadFromStream(Stream);
      fOwner.HTTPSend.Headers.Add( 'SoapAction: "urn:cepwsdl#ConsultaCEP"' );
      fOwner.HTTPPost(fpURL);

      ProcessaResposta;
    except
      on E: Exception do
      begin
        raise EACBrCEPException.Create(
          'Ocorreu o seguinte erro ao consumir o webService base4you:' + sLineBreak +
          '  - ' + E.Message
        );
      end;
    end;
  finally
    Stream.Free;
    Acao.Free;
  end;
end;

procedure TACBrWSBases4you.BuscarPorLogradouro(AMunicipio, ATipo_Logradouro,
  ALogradouro, AUF, ABairro: String);
begin
  inherited;

end;

procedure TACBrWSBases4you.ProcessaResposta;
begin

end;

{ TACBrWSRNSolucoes }
constructor TACBrWSRNSolucoes.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);

  fOwner.ParseText := False;
  fpURL := 'http://www.rnsolucoes.com/wsRN.php?type=xml';
end;

procedure TACBrWSRNSolucoes.BuscarPorCEP(ACEP: String);
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
      raise EACBrCEPException.Create(
        'Ocorreu o seguinte erro ao consumir o webService RNSolucoes:' + sLineBreak +
        '  - ' + E.Message
      );
    end;
  end;
end;

procedure TACBrWSRNSolucoes.BuscarPorLogradouro(AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
begin
  try
    fOwner.HTTPSend.Clear;

    //fOwner.AjustaParam(AMunicipio)
    //fOwner.AjustaParam(ALogradouro)

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
      raise EACBrCEPException.Create(
        'Ocorreu o seguinte erro ao consumir o webService RNSolucoes:' + sLineBreak +
        '  - ' + E.Message
      );
    end;
  end;
end;

procedure TACBrWSRNSolucoes.ProcessaResposta;
var
  Buffer: string;
  s: string;
  i: Integer;
  SL1: TStringList;
begin
  SL1 := TStringList.Create;

  try
    Buffer := fOwner.RespHTTP.Text;
    Buffer := StringReplace(Buffer, sLineBreak, '', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, '<dados>', '', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, '</dados>', '', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, '</dado>', '</dado>' + sLineBreak, [rfReplaceAll]);

    SL1.Text := Buffer;

    for i := 0 to SL1.Count-1 do
    begin
      s := SL1.Strings[i];

      if LerTagXML(s, 'cep') <> '' then
      begin
        with fOwner.Enderecos.New do
        begin
          CEP             := LerTagXML(Buffer, 'cep');
          Tipo_Logradouro := '';
          Logradouro      := LerTagXML(Buffer, 'endereco');
          Complemento     := LerTagXML(Buffer, 'complemento');
          Bairro          := LerTagXML(Buffer, 'bairro');
          Municipio       := LerTagXML(Buffer, 'cidade');
          UF              := LerTagXML(Buffer, 'uf');
          IBGE_Municipio  := '';
        end;
      end;
    end;
  finally
    SL1.Free;
  end;

  if Assigned(fOwner.OnBuscaEfetuada) then
    fOwner.OnBuscaEfetuada(Self);
end;


{ TACBrWSkingHost http://webservice.kinghost.net/*************}

constructor TACBrWSKingHost.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://webservice.kinghost.net/' ;
end;

procedure TACBrWSKingHost.BuscarPorCEP(ACEP: String);
begin
  FCepBusca := ACep;
  ACEP := OnlyNumber( ACEP );

  fOwner.HTTPGet( fpURL + 'web_cep.php?'+
                          'auth='+ Trim(fOwner.ChaveAcesso) +
                          '&formato=xml'+
                          '&cep='+ACEP ) ;
  ProcessaResposta ;
end;

procedure TACBrWSKingHost.BuscarPorLogradouro(AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
begin
  raise EACBrCEPException.Create(ACBrStr('Busca por Logradouro não disponível no site kingHost.'));
end;

procedure TACBrWSKingHost.ProcessaResposta;
var
  Buffer : String ;
begin
  fOwner.fEnderecos.Clear;

  Buffer := fOwner.RespHTTP.Text;
  if StrToIntDef(LerTagXML(Buffer, 'resultado'), 0) > 0 then
  begin
    with fOwner.Enderecos.New do
    begin
      CEP             := FCepBusca ; // kingHost nao devolve o cep na resposta
      Tipo_Logradouro := LerTagXML(Buffer,'tipo_logradouro') ;
      Logradouro      := LerTagXML(Buffer,'logradouro') ;
      Complemento     := LerTagXML(Buffer,'complemento') ;
      Bairro          := LerTagXML(Buffer,'bairro') ;
      Municipio       := LerTagXML(Buffer,'cidade') ;
      UF              := LerTagXML(Buffer,'uf') ;
      IBGE_Municipio  := '';
    end ;
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

procedure TACBrWSByJG.BuscarPorCEP(ACEP: String);
begin
  TestarUsuario;
  FCepBusca := ACEP;
  ACEP := OnlyNumber( ACEP );
  FTipoBusca := 1;
  fOwner.HTTPGet( fpURL + '?httpmethod=obterlogradouroauth&cep='+ACEP+
  '&usuario='+Trim(fOwner.Usuario)+'&senha='+Trim(fOwner.Senha)) ;
  ProcessaResposta ;
end;

procedure TACBrWSByJG.BuscarPorLogradouro(AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
var
  Endereco : String;
begin
  TestarUsuario;

  AMunicipio       := fOwner.AjustaParam( AMunicipio ) ;
  Endereco         := fOwner.AjustaParam(ATipo_Logradouro+' '+ALogradouro);
  AUF              := fOwner.AjustaParam( AUF );

  if (AMunicipio = '') or (Endereco = '') or (AUF = '') then
     raise EACBrCEPException.Create('UF, Cidade e Logradouro devem ser informados');

  FTipoBusca := 2;

  fOwner.HTTPGet( fpURL+'?httpmethod=obterCEPAuth&logradouro='+Endereco+
 '&localidade='+AMunicipio+'&UF='+AUF+'&usuario='+Trim(fOwner.Usuario)+'&senha='+Trim(fOwner.Senha)) ;
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
   Buffer, Resp, TLog: TStringList;
   TipoLogradouro, Logra, Comp: String;
   i, k : Integer;
begin
  Buffer := TStringList.Create;
  try
  ExtractStrings(['|'],[], PChar(fOwner.RespHTTP.Text), Buffer);

  i := CompareText(Buffer[1], ACBrStr('Cep '+FCepBusca+' não encontrado'));
  k := CompareText(Buffer[1], ACBrStr('CEP não está no formato 00000-000 ou 00000000'));

  if (i <> 0) and (k <> 0) then
  begin
    Resp := TStringList.Create;
    TLog := TStringList.Create;
		try
			Logra := '';
			Comp := '';
			Buffer[1] := StringReplace(Buffer[1], '''', '', [rfReplaceAll]);
			ExtractStrings([','],[], PChar(Buffer[1]), Resp);
			ExtractStrings([' '],[], PChar(Resp[0]), TLog);
			TipoLogradouro := Trim(TLog[0]);
			TLog.Clear;
			ExtractStrings(['-'],[], PChar(Resp[0]), TLog);
			Logra := Trim(TLog[0]);
			if(TLog.Count > 1) then
				Comp := Trim(TLog[1]);
			Delete(Logra, 1, Length(TipoLogradouro));

			with fOwner.Enderecos.New do
			begin
				CEP             := Trim(FCepBusca);
				Tipo_Logradouro := Trim(TipoLogradouro);
				Logradouro      := Trim(Logra);
				Complemento     := Trim(Comp);
				Bairro          := Trim(Resp[1]);
				Municipio       := Trim(Resp[2]);
				UF              := Trim(Resp[3]);
				IBGE_Municipio  := Trim(Resp[4]);
			end;
		finally
			Resp.Free;
			TLog.Free;
		end;
  end ;
  finally
    if Assigned(Buffer) then
      Buffer.Free;
  end;

  if Assigned( fOwner.OnBuscaEfetuada ) then
    fOwner.OnBuscaEfetuada( Self );
end ;

procedure TACBrWSByJG.ProcessaLogradouro;
var
   Buffer, Resp, TLog: TStringList;
   Qtd, i, k : Integer;
   TipoLogradouro, Logra, Comp: String;
begin
  Buffer := TStringList.Create;
  try
    ExtractStrings(['|'],[], PChar(fOwner.RespHTTP.Text), Buffer);
    Qtd := StrToInt(Buffer[1]);
    i := CompareText(Buffer[2], ACBrStr('Logradouro não encontrado'));
    k := 2;

    if i <> 0 then
    begin
      Resp := TStringList.Create;
      TLog := TStringList.Create;

      try
        for i := 1 to Qtd do
        begin
          Logra := '';
          Comp := '';

          ExtractStrings([','],[], PChar(Buffer[k]), Resp);

          if CompareText(Resp[0], ACBrStr('00000000')) = 0 then
            Break;

          ExtractStrings([' '],[], PChar(Resp[1]), TLog);
          TipoLogradouro := Trim(TLog[0]);
          TLog.Clear;
          ExtractStrings(['-'],[], PChar(Resp[1]), TLog);
          Logra := Trim(TLog[0]);
          if(TLog.Count > 1) then
            Comp := Trim(TLog[1]);
          Delete(Logra, 1, Length(TipoLogradouro));

          with fOwner.Enderecos.New do
          begin
            CEP             := Trim(Resp[0]);
            Tipo_Logradouro := Trim(TipoLogradouro);
            Logradouro      := Trim(Logra);
            Complemento     := Trim(Comp);
            Bairro          := Trim(Resp[2]);
            Municipio       := Trim(Resp[3]);
            UF              := Trim(Resp[4]);
            IBGE_Municipio  := Trim(Resp[5]);
          end;

          Resp.Clear;
          TLog.Clear;

          Inc(k);
        end;
      finally
        Resp.Free;
        TLog.Free;
      end ;
    end;

    if Assigned( fOwner.OnBuscaEfetuada ) then
      fOwner.OnBuscaEfetuada( Self );

  finally
    Buffer.Free;
  end;
end;

{ TACBrWSCorreios }

constructor TACBrWSCorreios.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://www.buscacep.correios.com.br/servicos/dnec/consultaEnderecoAction.do?' ;
  fACBrIBGE := TACBrIBGE.Create(nil);
end;

destructor TACBrWSCorreios.Destroy ;
begin
  fACBrIBGE.Free;
  inherited Destroy ;
end ;

procedure TACBrWSCorreios.BuscarPorCEP(ACEP: String);
var
  sParams: string;
begin
  ACEP := OnlyNumber( ACEP );
  sParams := 'relaxation='+ACEP+'&TipoCep=ALL&cfm=1&Metodo=listaLogradouro&TipoConsulta=relaxation&StartRow=1&EndRow=100';
  fOwner.HTTPGet(fpURL + sParams);
  ProcessaResposta;
end;

procedure TACBrWSCorreios.BuscarPorLogradouro( AMunicipio, ATipo_Logradouro, ALogradouro,
         AUF, ABairro : String ) ;
var
  sParams: string;
  sEndereco: String;

  Procedure AddParam( AParam: String; Separador: String = '/' ) ;
  begin
    if (Trim(AParam) <> '') then
       sEndereco := sEndereco + Separador + AParam;
  end ;

begin
  if trim(AMunicipio) = '' then
     raise EACBrCEPException.Create( ACBrStr('Você deve informar o Município') );

  if trim(AUF) = '' then
     raise EACBrCEPException.Create( ACBrStr('Você deve informar a UF') );

   sEndereco := '';
   AddParam( ATipo_Logradouro, '' );
   AddParam( ALogradouro, ' ' );
   AddParam( ABairro );
   AddParam( AMunicipio );
   AddParam( AUF );

   if trim(sEndereco) = '' then
      raise EACBrCEPException.Create(ACBrStr('Não existe Parametros para Pesquisa'));

   //sEndereco := ATipo_Logradouro+' '+ALogradouro+ '/'+ABairro+'/'+AMunicipio+'/'+AUF;

   sParams := 'relaxation='+EncodeURL( ACBrStrToAnsi(sEndereco) )+
              '&TipoCep=ALL&semelhante=N&cfm=1&Metodo=listaLogradouro&TipoConsulta=relaxation&StartRow=1&EndRow=10';
   fOwner.HTTPGet(fpURL + sParams);
   ProcessaResposta;
end;

procedure TACBrWSCorreios.ProcessaResposta;
var
  iLin, iPos, iEnd, iFim : Integer;
  sLin, sMun : string;
  SL : TStringList;
begin
  fOwner.fEnderecos.Clear;

  fACBrIBGE.ProxyHost := fOwner.ProxyHost;
  fACBrIBGE.ProxyPort := fOwner.ProxyPort;
  fACBrIBGE.ProxyPass := fOwner.ProxyPass;
  fACBrIBGE.ProxyUser := fOwner.ProxyUser;

  SL := TStringList.Create;
  Try
    // Limpando HTML //
    SL.Text := StripHTML( fOwner.RespHTTP.Text );
    RemoveEmptyLines( SL );

    //DEBUG
    //SL.SaveToFile('c:\temp\bobo.txt');

    iLin := 0;
    iFim := SL.Count-1;
    while iLin < iFim do
    begin
      sLin := SL[iLin] ;

      if Pos('Resultado superior a 100', sLin) > 0 then
        raise EACBrCEPException.Create('Resultado superior a 100 registros');

      if (Pos( ACBrStr('O endereço informado'), sLin) > 0) and
         (Pos( ACBrStr('não foi encontrado'), sLin) > 0) then
        break ;

      if Pos('Logradouro(s)', sLin) > 0 then       // Aqui começam os endereços
      begin
        iEnd := StrToIntDef( OnlyNumber(sLin), 0) ;
        if iEnd < 1 then
          Break;

        iLin := iLin + 6 ;  // Pulando linhas do cabeçalho
        sMun := '';

        // Achando a linha final //
        iFim := iLin;
        while (iFim < SL.Count) and (Pos( ACBrStr('Para mais informações,'), SL[iFim]) = 0) do
           Inc( iFim );

        while iLin < iFim do
        begin
          { Se tem 1 endereço apenas, verifica se retornou Logradouro.
            Ex: "78075-990" é o CEP de uma caixa comunitária }
          iPos := IfThen( iEnd = 1, 5-(iFim - iLin), 0 ) ;

          with fOwner.Enderecos.New do
          begin
            while (iLin < iFim) and (iPos < 5) do
            begin
              sLin := Trim(SL[iLin]) ;

              Case iPos of
                0 :
                  begin
                    Tipo_Logradouro := Trim(Copy(sLin,1,Pos(' ',sLin)));
                    Logradouro      := Trim(Copy(sLin,Pos(' ',sLin),Length(sLin)));
                    Complemento     := '';
                  end ;
                1 : Bairro    := sLin ;
                2 : Municipio := sLin ;
                3 : UF        := sLin ;
                4 : CEP       := sLin;
              end ;

              Inc( iPos );
              Inc( iLin );
            end ;

            // Correios não retornam informação do IBGE, Fazendo busca do IBGE com ACBrIBGE //
            IBGE_Municipio := '';
            if (Municipio <> '') and
               (fOwner.PesquisarIBGE) then
            begin
              if (sMun <> Municipio) then  // Evita buscar municipio já encontrado
              begin
                fACBrIBGE.BuscarPorNome( Municipio, UF, True);
                sMun := Municipio;
              end ;

              if fACBrIBGE.Cidades.Count > 0 then  // Achou ?
                 IBGE_Municipio := IntToStr( fACBrIBGE.Cidades[0].CodMunicipio );
            end ;
          end ;
        end ;
      end
      else
        Inc( iLin );
    end;
  finally
    SL.Free;
  end ;

  if Assigned( fOwner.OnBuscaEfetuada ) then
    fOwner.OnBuscaEfetuada( Self );
end;

{ TACBrWSDevMedia http://www.devmedia.com.br/devware/cep/service/ *************}

constructor TACBrWSDevMedia.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fpURL := 'http://www.devmedia.com.br/api/cep/service/';
end;

procedure TACBrWSDevMedia.BuscarPorCEP(ACEP: String);
begin
  TestarChave;

  FCepBusca := ACep;
  ACEP := OnlyNumber( ACEP );

  fOwner.HTTPGet( fpURL + '?cep=' + ACEP + '&chave=' + Trim(fOwner.ChaveAcesso) + '&formato=xml' ) ;
  ProcessaResposta;
end;

procedure TACBrWSDevMedia.BuscarPorLogradouro(AMunicipio,
  ATipo_Logradouro, ALogradouro, AUF, ABairro: String);
begin
  raise EACBrCEPException.Create(ACBrStr('Busca por Logradouro não disponível no site DevMedia.'));
end;

procedure TACBrWSDevMedia.ProcessaResposta;
var
  Resultado, Buffer : String ;
begin
  fOwner.fEnderecos.Clear;

  Buffer    := fOwner.RespHTTP.Text;
  Resultado := LerTagXML(Buffer, 'resultado_txt') ;

  if Resultado = 'sucesso' then
  begin
    with fOwner.Enderecos.New do
    begin
      CEP             := FCepBusca ; // DEVMEDIA nao devolve o cep na resposta
      Tipo_Logradouro := LerTagXML(Buffer,'tipo_logradouro') ;
      Logradouro      := LerTagXML(Buffer,'logradouro') ;
      Complemento     := LerTagXML(Buffer,'complemento') ;
      Bairro          := LerTagXML(Buffer,'bairro') ;
      Municipio       := LerTagXML(Buffer,'cidade') ;
      UF              := LerTagXML(Buffer,'uf') ;
      IBGE_Municipio  := LerTagXML(Buffer,'codigomunicipio');
    end ;
  end
  else
     raise EACBrCEPException.Create( ACBrStr(Resultado) );

  if Assigned( fOwner.OnBuscaEfetuada ) then
    fOwner.OnBuscaEfetuada( Self );
end ;

procedure TACBrWSDevMedia.TestarChave;
begin
  if fOwner.ChaveAcesso = EmptyStr then
    raise EACBrCEPException.Create( ACBrStr('Chave de acesso não informada.') );
end;

{ TACBrWSViaCEP }

constructor TACBrWSViaCEP.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fOwner.ParseText := True;
  fpURL := 'http://viacep.com.br/ws/';
end;

procedure TACBrWSViaCEP.BuscarPorCEP(ACEP: String);
begin
  FCepBusca := ACep;
  ACEP := OnlyNumber( ACEP );

  if ACEP = '' then
     raise EACBrCEPException.Create('CEP deve ser informado');

  fOwner.HTTPGet( fpURL + ACEP + '/xml' ) ;
  ProcessaResposta();
end;

procedure TACBrWSViaCEP.BuscarPorLogradouro(AMunicipio, ATipo_Logradouro,ALogradouro, AUF, ABairro: String);
begin
  if AMunicipio = '' then
     raise EACBrCEPException.Create('Munícipio deve ser informado.');

  if ALogradouro = '' then
     raise EACBrCEPException.Create('Logradouro deve ser informado.');

  if AUF = '' then
     raise EACBrCEPException.Create('UF deve ser informado.');

  AUF         := TiraAcentos(AUF);
  AMunicipio  := TiraAcentos(AMunicipio);
  ALogradouro := TiraAcentos(ALogradouro);

  fOwner.HTTPGet( fpURL + AUF + '/' + AMunicipio + '/' + ALogradouro + '/xml' );
  ProcessaResposta();
end;

procedure TACBrWSViaCEP.ProcessaResposta;
var
  Buffer: string;
  s: string;
  i: Integer;
  SL1: TStringList;
begin
  SL1 := TStringList.Create;

  try
    Buffer := fOwner.RespHTTP.Text;
    Buffer := StringReplace(Buffer, sLineBreak, '', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, '<enderecos>', '', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, '</enderecos>', '', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, '</endereco>', '</endereco>' + sLineBreak, [rfReplaceAll]);

    SL1.Text := Buffer;

    for i := 0 to SL1.Count-1 do
    begin
      s := SL1.Strings[i];

      if LerTagXML(s, 'cep') <> '' then
      begin
        with fOwner.Enderecos.New do
        begin
          CEP             := LerTagXML(s, 'cep');
          Tipo_Logradouro := '';
          Logradouro      := LerTagXML(s, 'logradouro');
          Complemento     := LerTagXML(s, 'complemento');
          Bairro          := LerTagXML(s, 'bairro');
          Municipio       := LerTagXML(s, 'localidade');
          UF              := LerTagXML(s, 'uf');
          IBGE_Municipio  := LerTagXML(s, 'ibge');
        end;
      end;
    end;
  finally
    SL1.Free;
  end;

  if Assigned(fOwner.OnBuscaEfetuada) then
    fOwner.OnBuscaEfetuada(Self);
end;



{ TACBrWSCorreiosSIGEP }
constructor TACBrWSCorreiosSIGEP.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);
  fACBrIBGE := TACBrIBGE.Create(nil);

  fOwner.ParseText := False;
  fpURL := 'https://apps.correios.com.br/SigepMasterJPA/AtendeClienteService/AtendeCliente?wsdl';
end;

destructor TACBrWSCorreiosSIGEP.Destroy;
begin
  fACBrIBGE.Free;
  inherited Destroy;
end;

procedure TACBrWSCorreiosSIGEP.BuscarPorCEP(ACEP: String);
var
  Acao: TStringList;
  Stream: TMemoryStream;
begin
  Acao   := TStringList.Create;
  Stream := TMemoryStream.Create;
  try
    Acao.Text :=
     '<?xml version="1.0" encoding="UTF-8" standalone="no"?>'+
     '<soapenv:Envelope xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/" '+
     'xmlns:cli="http://cliente.bean.master.sigep.bsb.correios.com.br/"> '+
     ' <soapenv:Header/>'+
     ' <soapenv:Body>' +
     ' <cli:consultaCEP>' +
     ' <cep>' + ACEP + '</cep>' +
     ' </cli:consultaCEP>' +
     ' </soapenv:Body>' +
     ' </soapenv:Envelope>';

    try
      Acao.SaveToStream(Stream);

      fOwner.HTTPSend.Clear;
      fOwner.HTTPSend.Document.LoadFromStream(Stream);
      fOwner.HTTPPost(fpURL);

      ProcessaResposta;
    except
      on E: Exception do
      begin
        if Pos('CEP NAO ENCONTRADO', E.Message) <> 0  then
          raise EACBrCEPException.Create('CEP NAO ENCONTRADO')
        else
          raise EACBrCEPException.Create(
            'Ocorreu o seguinte erro ao consumir o WebService dos correios:' + sLineBreak +
            '  - ' + E.Message
          );
      end;
    end;
  finally
    Stream.Free;
    Acao.Free;
  end;
end;

procedure TACBrWSCorreiosSIGEP.ProcessaResposta;
var
  Buffer: string;
  s: string;
  i: Integer;
  SL1: TStringList;
  sMun, Complemento2: String;
begin
  SL1 := TStringList.Create;

  fACBrIBGE.ProxyHost := fOwner.ProxyHost;
  fACBrIBGE.ProxyPort := fOwner.ProxyPort;
  fACBrIBGE.ProxyPass := fOwner.ProxyPass;
  fACBrIBGE.ProxyUser := fOwner.ProxyUser;

  try
    Buffer := fOwner.RespHTTP.Text;
    Buffer := StringReplace(Buffer, sLineBreak, '', [rfReplaceAll]);

    SL1.Text := Buffer;
    sMun := '';

    for i := 0 to SL1.Count-1 do
    begin
      s := SL1.Strings[i];

      if LerTagXML(s, 'cep') <> '' then
      begin
        with fOwner.Enderecos.New do
        begin
          CEP             := LerTagXML(Buffer, 'cep');
          Tipo_Logradouro := '';
          Logradouro      := LerTagXML(Buffer, 'end');
          Complemento     := Trim(LerTagXML(Buffer, 'complemento'));
          Complemento2    := Trim(LerTagXML(Buffer, 'complemento2'));
          Bairro          := LerTagXML(Buffer, 'bairro');
          Municipio       := LerTagXML(Buffer, 'cidade');
          UF              := LerTagXML(Buffer, 'uf');
          IBGE_Municipio  := '';

          if Complemento2 <> '' then
          begin
            if Complemento <> ''  then
              Complemento := Complemento + ' ';

            Complemento := Complemento + Complemento2;
          end;

          // Correios não retornam informação do IBGE, Fazendo busca do IBGE com ACBrIBGE //
          if (Municipio <> '') and
             (fOwner.PesquisarIBGE) then
          begin
            if (sMun <> Municipio) then  // Evita buscar municipio já encontrado
            begin
              fACBrIBGE.BuscarPorNome( Municipio, UF, True) ;
              sMun := Municipio;
            end ;

            if fACBrIBGE.Cidades.Count > 0 then  // Achou ?
               IBGE_Municipio := IntToStr( fACBrIBGE.Cidades[0].CodMunicipio );
          end ;

        end;
      end;
    end;
  finally
    SL1.Free;
  end;

  if Assigned(fOwner.OnBuscaEfetuada) then
    fOwner.OnBuscaEfetuada(Self);
end;

procedure TACBrWSCEPAberto.BuscarPorLogradouro(AMunicipio, ATipo_Logradouro,
  ALogradouro, AUF, ABairro: String);
var
  Parametros: String;
begin
  if(Trim(fOwner.ChaveAcesso) = '')then
    raise EACBrCEPException.Create('O WebService CepAberto necessita de uma Chave de Acesso.'#13+
                                   'Acesse o site, crie uma conta e pegue sua Chave de Acesso em APIKey!');

  if AUF = '' then
     raise EACBrCEPException.Create('UF deve ser informado.');

  if AMunicipio = '' then
     raise EACBrCEPException.Create('Munícipio deve ser informado.');

  AUF         := fOwner.AjustaParam(AUF);
  AMunicipio  := fOwner.AjustaParam(AMunicipio);
  ALogradouro := fOwner.AjustaParam(ALogradouro);
  ABairro     := fOwner.AjustaParam(ABairro);

  // estado e cidade são obrigatórios
  Parametros := 'estado=' + AUF +
                '&cidade=' + AMunicipio;

  if(Trim(ABairro) <> '')then
    Parametros := Parametros + '&bairro=' + ABairro;

  if(Trim(ALogradouro) <> '')then
    Parametros := Parametros + '&logradouro=' + ALogradouro;

  fOwner.HTTPSend.Clear;
  fOwner.HTTPSend.Headers.Add('Authorization: Token token="'+fOwner.ChaveAcesso+'"');
  fOwner.HTTPMethod('GET', fpURL+'estado' + Parametros);

  ProcessaResposta;
end;

constructor TACBrWSCEPAberto.Create(AOwner: TACBrCEP);
begin
  inherited Create(AOwner);

  fOwner.ParseText := False;
  fpURL := 'http://www.cepaberto.com/api/v3/';
end;

procedure TACBrWSCEPAberto.BuscarPorCEP(ACEP: String);
begin
  ACEP := OnlyNumber( ACEP );

  if ACEP = '' then
    raise EACBrCEPException.Create('CEP deve ser informado');

  if(Trim(fOwner.ChaveAcesso) = '')then
    raise EACBrCEPException.Create('O WebService CepAberto necessita de uma Chave de Acesso.'+#13+'Acesse o site, crie uma conta e pegue sua Chave de Acesso em APIKey!');

  fOwner.HTTPSend.Clear;
  fOwner.HTTPSend.Headers.Add('Authorization: Token token="'+fOwner.ChaveAcesso+'"');
  fOwner.HTTPMethod('GET', fpURL+'cep?cep=' + ACEP);

  ProcessaResposta;
end;

procedure TACBrWSCEPAberto.ProcessaResposta;
var
  Buffer: string;
  s: string;
  i: Integer;
  SL1: TStringList;

  function LerTagJson(AJsonStr, ATag: String): string;
  var
    VJson: TJson;
    VTag, VTag2: string;
  begin
    VJson := TJson.Create;
    try
      VJson.Parse(AJsonStr);

      if(Pos('->', ATag) > 0)then
      begin
        VTag := LeftStr(ATag, Pos('->', ATag)-1);
        VTag2 := Copy(ATag, Length(VTag)+3, MaxInt);
        ATag := VTag;
      end;

      //Se for um objeto de valores, entra na recursao
      if(VJson.Get(ATag).ValueType = jvObject)then
        Result := LerTagJson(VJson.Get(ATag).Stringify, VTag2)
      else
        Result := VJson.Get(ATag).AsString;
    finally
      VJson.Free;
    end;
  end;

begin
  SL1 := TStringList.Create;
  try
    Buffer := fOwner.RespHTTP.Text;

    Buffer := StringReplace(Buffer, '<?xml version="1.0" encoding="UTF-8"?>'+ sLineBreak+'<cep>', '<?xml version="1.0" encoding="UTF-8"?>'+ sLineBreak+'<resposta>', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, '</estado>'+ sLineBreak+'</cep>', '</estado>'+ sLineBreak+'</resposta>', [rfReplaceAll]);
    Buffer := StringReplace(Buffer, sLineBreak, '', [rfReplaceAll]);

    SL1.Text := Buffer;

    for i := 0 to SL1.Count-1 do
    begin
      s := SL1.Strings[i];

      if LerTagJson(s, 'cep') <> '' then
      begin
        with fOwner.Enderecos.New do
        begin
          CEP             := LerTagJson(s, 'cep');
          Logradouro      := LerTagJson(s, 'logradouro');
          Complemento     := LerTagJson(s, 'complemento');
          Bairro          := LerTagJson(s, 'bairro');
          Municipio       := LerTagJson(s, 'cidade->nome');
          UF              := LerTagJson(s, 'estado->sigla');
          IBGE_Municipio  := LerTagJson(s, 'cidade->ibge');
          Altitude        := LerTagJson(s, 'altitude');
          Latitude        := LerTagJson(s, 'latitude');
          Longitude       := LerTagJson(s, 'longitude');
        end;
      end;
    end;
  finally
    SL1.Free;
  end;

  if Assigned(fOwner.OnBuscaEfetuada) then
    fOwner.OnBuscaEfetuada(Self);
end;

{ TACBrWSWSCEP }

procedure TACBrWSWSCEP.BuscarPorCEP(ACEP: String);
begin
  ACEP := OnlyNumber( ACEP );

  if ACEP = '' then
    raise EACBrCEPException.Create('CEP deve ser informado');

  if(Trim(fOwner.ChaveAcesso) = '')then
    raise EACBrCEPException.Create('O WebService WSCep necessita de uma Chave de Acesso.'+#13+'Acesse o site, crie uma conta e pegue sua Chave de Acesso! Use a chave "free" para ter acesso ate 30 consultas por dia');

  fOwner.HTTPSend.Clear;
  fOwner.HTTPMethod('GET', Format(fpURL, [fOwner.ChaveAcesso, ACEP]));

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
  Buffer: string;
begin
  Buffer := fOwner.RespHTTP.Text;

  Buffer := StringReplace(Buffer, '<?xml version="1.0" encoding="UTF-8"?>'+ sLineBreak+'<cep>', '<?xml version="1.0" encoding="UTF-8"?>'+ sLineBreak+'<resposta>', [rfReplaceAll]);
  Buffer := StringReplace(Buffer, '</estado>'+ sLineBreak+'</cep>', '</estado>'+ sLineBreak+'</resposta>', [rfReplaceAll]);
  Buffer := StringReplace(Buffer, sLineBreak, '', [rfReplaceAll]);

  if LerTagXML(Buffer, 'cep') <> '' then
  begin
    with fOwner.Enderecos.New do
    begin
      CEP             := LerTagXML(Buffer, 'cep');
      Logradouro      := LerTagXML(Buffer, 'logradouro');
      Bairro          := LerTagXML(Buffer, 'bairro');
      Municipio       := LerTagXML(Buffer, 'cidade');
      UF              := LerTagXML(Buffer, 'uf');
      IBGE_Municipio  := LerTagXML(Buffer, 'cod_ibge_municipio');
      Altitude        := LerTagXML(Buffer, 'alt');
      Latitude        := LerTagXML(Buffer, 'lat');
      Longitude       := LerTagXML(Buffer, 'lng');
    end;
  end;

  if Assigned(fOwner.OnBuscaEfetuada) then
    fOwner.OnBuscaEfetuada(Self);
end;

end.
