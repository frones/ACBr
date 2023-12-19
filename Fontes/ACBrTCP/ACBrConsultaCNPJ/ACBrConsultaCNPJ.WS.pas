unit ACBrConsultaCNPJ.WS;

interface
uses
  ACBrJSON, SysUtils, ACBrValidador, httpsend,
  Classes;
type
  EACBrConsultaCNPJWSException = class ( Exception );
  TACBrConsultaCNPJWSResposta = class (TObject)
    NaturezaJuridica     : String ;
    EmpresaTipo          : String;
    Abertura             : TDateTime;
    RazaoSocial          : String;
    Fantasia             : String;
    Porte                : String;
    CNAE1                : String;
    CNAE2                : TStringList;
    Endereco             : String;
    Numero               : String;
    Complemento          : String;
    CEP                  : String;
    Bairro               : String;
    Cidade               : String;
    UF                   : String;
    Situacao             : String;
    SituacaoEspecial     : String;
    CNPJ                 : String;
    DataSituacao         : TDateTime;
    DataSituacaoEspecial : TDateTime;
    EndEletronico        : String;
    Telefone             : String;
    EFR                  : string;
    MotivoSituacaoCad    : string;
    CodigoIBGE           : String;
  end;
  { TACBrConsultaCNPJWS }
  TACBrConsultaCNPJWS = class( TObject )
    FCNPJ : string;
    FUsuario : String;
    FSenha : String;
    FResposta : TACBrConsultaCNPJWSResposta;
  private
    FHTTPSend: THTTPSend;
    public
      constructor create(const ACNPJ : string; AUsuario : string = ''; ASenha: string = '');
      destructor Destroy; override;
      function Executar : boolean; virtual;
      property HTTPSend: THTTPSend read FHTTPSend write FHTTPSend;
  end;
implementation

{ TACBrConsultaCNPJWS }

constructor TACBrConsultaCNPJWS.create(const ACNPJ : string; AUsuario : string = ''; ASenha: string = '');
begin
  FCNPJ     :=  ACNPJ;
  FUsuario  := AUsuario;
  FSenha    := ASenha;
  FResposta := TACBrConsultaCNPJWSResposta.Create;
  FResposta.CNAE2     := TStringList.Create;
end;

destructor TACBrConsultaCNPJWS.Destroy;
begin
  FResposta.CNAE2.Free;
  FResposta.Free;
  inherited;
end;

function TACBrConsultaCNPJWS.Executar: boolean;
var LErro : String;
begin
  Result := False;
  LErro := ValidarCNPJ( FCNPJ ) ;
  if LErro <> '' then
    raise EACBrConsultaCNPJWSException.Create(LErro);
end;

end.
