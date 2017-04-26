unit pcnMFeUtil;

interface

uses
  Classes, SysUtils, pcnGerador, pcnConversao;

type

  { TIdentificador }
  TIdentificador = class(TPersistent)
  private
    FGerador: TGerador;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarIdentificador( Identificador : String );
  end;

  { TMetodo }
  TMetodo = class(TPersistent)
  private
    FGerador: TGerador;
    FIdentificador: TIdentificador;
    FAdicionarParametros : Boolean;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarMetodo( Valor : Integer; Componente, Metodo : String );
    procedure FinalizarMetodo;

    property AdicionarParametros : Boolean read FAdicionarParametros write FAdicionarParametros default True;
  end;

  { TConstrutor }
  TConstrutor = class(TPersistent)
  private
    FGerador: TGerador;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarConstructor( Nome, Valor: String );
  end;

  { TParametro }
  TParametro = class(TPersistent)
  private
    FGerador: TGerador;
  public
    constructor Create(AOwner: TGerador);
    destructor Destroy; override;
    procedure GerarParametro( Nome: String; Valor: Variant; Tipo: TpcnTipoCampo; ParseTextoXML: Boolean = True );
  end;


implementation

{ TMetodo }

constructor TMetodo.Create(AOwner: TGerador);
begin
  FGerador := AOwner;
  FIdentificador := TIdentificador.Create(FGerador);
end;

destructor TMetodo.Destroy;
begin
  FIdentificador.Free;
  inherited Destroy;
end;

procedure TMetodo.GerarMetodo(Valor: Integer; Componente, Metodo: String);
begin
  FGerador.wGrupo('Integrador');
  FIdentificador.GerarIdentificador(IntToStr(Valor));
  FGerador.wGrupo('Componente Nome="'+Componente+'"');
  FGerador.wGrupo('Metodo Nome="'+Metodo+'"');
  if AdicionarParametros then
    FGerador.wGrupo('Parametros');
end;

procedure TMetodo.FinalizarMetodo;
begin
  if AdicionarParametros then
    FGerador.wGrupo('/Parametros');
  FGerador.wGrupo('/Metodo');
  FGerador.wGrupo('/Componente');
  FGerador.wGrupo('/Integrador');
end;

{ TParametro }

constructor TParametro.Create(AOwner: TGerador);
begin
  FGerador := AOwner
end;

destructor TParametro.Destroy;
begin
  inherited Destroy;
end;

procedure TParametro.GerarParametro(Nome: String; Valor: Variant;
  Tipo: TpcnTipoCampo; ParseTextoXML: Boolean);
begin
  FGerador.wGrupo('Parametro');
  FGerador.wCampo(tcStr, '', 'Nome', 1, 99, 1, Nome, 'Nome do Parâmetro');
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Valor, 'Valor do Parâmetro',ParseTextoXML);
  FGerador.wGrupo('/Parametro');
end;

{ TConstrutor }

constructor TConstrutor.Create(AOwner: TGerador);
begin
   FGerador := AOwner
end;

destructor TConstrutor.Destroy;
begin
  inherited Destroy;
end;

procedure TConstrutor.GerarConstructor(Nome, Valor: String);
begin
  FGerador.wGrupo('Construtor');
  FGerador.wGrupo('Parametros');
  FGerador.wGrupo('Parametro');
  FGerador.wCampo(tcStr, '', 'Nome', 1, 99, 1, Nome, 'Nome do Construtor');
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Valor, 'Valor do Construtor');
  FGerador.wGrupo('/Parametro');
  FGerador.wGrupo('/Parametros');
  FGerador.wGrupo('/Construtor');
end;

{ TIdentificador }

constructor TIdentificador.Create(AOwner: TGerador);
begin
  FGerador := AOwner;
end;

destructor TIdentificador.Destroy;
begin
  inherited Destroy;
end;

procedure TIdentificador.GerarIdentificador(Identificador: String);
begin
  FGerador.wGrupo('Identificador');
  FGerador.wCampo(tcStr, '', 'Valor', 1, 99, 1, Identificador, 'Valor do Identificador');
  FGerador.wGrupo('/Identificador');
end;

end.

