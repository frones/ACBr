{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2014 Daniel Simoes de Almeida               }
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
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 25/03/2017: Laércio S Amici | Emerson Virissimo da Silva - www.viaregra.com
|*  - Primeira Versao ACBrFrenet
|*
|* 21/09/2018:
|*  - Novo logo Frenet; Ajuste no exemplo
|*
|* 08/02/2019:
|*  - Refactor usando unit "jsons.pas"
|*
|* 15/06/2020:
|*  - Nova propriedade de envio: Diameter
|*  - Novas propriedades no retorno
|*  - Novo método ServicosDisponiveis - retorna os serviços habilitados no painel Frenet
|*
|*
|*
|*  O Frenet é um serviço de cotação de fretes com ampla gama de transportadoras
|*  (incluindo os correios).
|*  O serviço funciona através da criação de uma conta em
|*  https://painel.frenet.com.br e obtendo um token.
|*  O Frenet possui diversos planos, começando pelo Free, conforme o volume de
|*  cotações disparadas no mês.
|*
|*  Documentação da API Frenet: http://docs.frenetapi.apiary.io/
******************************************************************************}

{$I ACBr.inc}

unit ACBrFrenet;

interface

uses
  Classes, SysUtils, contnrs,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
    JsonDataObjects_ACBr,
  {$Else}
    Jsons,
  {$EndIf}
  ACBrSocket,
  ACBrUtil.Strings;

const
  CURL_FRENET         = 'http://api.frenet.com.br';
  CURL_FRENET_QUOTE   = '/shipping/quote';
  CURL_FRENET_INFO    = '/shipping/info';

type

  { EACBrFrenetException }
  EACBrFrenetException = class(Exception)
  public
    constructor CreateACBrStr(const msg: string);
  end;


  { TACBrFrenetItem }

  TACBrFrenetItem = class
  private
    FLargura: Double;
    FComprimento: Double;
    FPeso: Double;
    FAltura: Double;
    FSKU: string;
    FCategoria: string;
    FQuantidade: integer;
    FDiametro: Double;
    procedure SetAltura(const Value: Double);
    procedure SetCategoria(const Value: string);
    procedure SetComprimento(const Value: Double);
    procedure SetLargura(const Value: Double);
    procedure SetPeso(const Value: Double);
    procedure SetQuantidade(const Value: integer);
    procedure SetSKU(const Value: string);
    procedure SetDiametro(const Value: Double);
  public
    constructor Create;
    property Quantidade: integer      read FQuantidade      write SetQuantidade;
    property Peso: Double             read FPeso            write SetPeso;
    property Comprimento: Double      read FComprimento     write SetComprimento;
    property Altura: Double           read FAltura          write SetAltura;
    property Largura: Double          read FLargura         write SetLargura;
    property Diametro: Double         read FDiametro        write SetDiametro;
    property SKU: string              read FSKU             write SetSKU;
    property Categoria: string        read FCategoria       write SetCategoria;
  end;

  
  { TACBrFrenetItens }

  TACBrFrenetItens = class(TObjectList)
  private
    procedure SetItem(Index: integer; Item: TACBrFrenetItem);
    function GetItem(Index: integer): TACBrFrenetItem;
  public
    procedure Insert(Index: integer; AObject: TACBrFrenetItem);
    function Add(AObject: TACBrFrenetItem): integer;
    function New: TACBrFrenetItem;

    property Items[Index: integer]: TACBrFrenetItem read GetItem write SetItem;  default;
  end;


  { TACBrFrenetCotacao }

  TACBrFrenetCotacao = class
  private
    FValor: Double;
    FPrazoEntrega: integer;
    FMensagem: string;
    FTransportador: string;
    FDescricaoServico: string;
    FCodigoServico: string;
    FErro: boolean;
    FValorOriginal: Double;
    FTempoResposta: Double;
    FCodigoTransportador: string;
    FPrazoEntregaOriginal: integer;
  public
    property Transportador: string           read FTransportador         write FTransportador;
    property CodigoTransportador: string     read FCodigoTransportador   write FcodigoTransportador;
    property PrazoEntrega: integer           read FPrazoEntrega          write FPrazoEntrega;
    property PrazoEntregaOriginal: integer   read FPrazoEntregaOriginal  write FPrazoEntregaOriginal;
    property Mensagem: string                read FMensagem              write FMensagem;
    property CodigoServico: string           read FCodigoServico         write FCodigoServico;
    property DescricaoServico: string        read FDescricaoServico      write FDescricaoServico;
    property Valor: Double                   read FValor                 write FValor;
    property ValorOriginal: Double           read FValorOriginal         write FValorOriginal;
    property Erro: boolean                   read FErro                  write FErro;
    property TempoResposta: Double           read FTempoResposta         write FTempoResposta;
  end;

  { TACBrFrenetCotacoes }

  TACBrFrenetCotacoes = class(TObjectList)
  protected
    procedure SetObject(Index: integer; Item: TACBrFrenetCotacao);
    function GetObject(Index: integer): TACBrFrenetCotacao;
  public
    function Add(Obj: TACBrFrenetCotacao): integer;
    function New: TACBrFrenetCotacao;

    property Objects[Index: integer]: TACBrFrenetCotacao read GetObject write SetObject;  default;
  end;



  { TACBrFrenet }

  TACBrFrenet = class(TACBrHTTP)
  private
    FUrlConsulta: string;

    FErro: Integer;
    FMsgErro: String;

    FItens: TACBrFrenetItens;
    FValorDeclarado: Double;
    FToken: string;
    FPaisDestino: string;
    FCepDestino: string;
    FCepOrigem: string;
    FCotacoes: TACBrFrenetCotacoes;
    FJson: TJsonObject;

    procedure SetItens(const Value: TACBrFrenetItens);
    function TemItens: Boolean;
    function UrlConsultaAlterada: Boolean;

  protected
    procedure LerCotacoes;
    procedure LerServicos;
    procedure LerMsgErro;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Realiza cotação de frete }
    function CotarFrete: boolean;

    { Buscar os servicos disponiveis - configurados no painel do Frenet }
    function ServicosDisponiveis: boolean;

    property Json: TJsonObject   read FJson;

    property Erro: integer       read FErro           write FErro;
    property MsgErro: string     read FMsgErro        write FMsgErro;

    property Cotacoes: TACBrFrenetCotacoes  read FCotacoes;

  published
    property Token: string            read FToken         write FToken;

    property CepOrigem: string        read FCepOrigem       write FCepOrigem;
    property CepDestino: string       read FCepDestino      write FCepDestino;
    property PaisDestino: string      read FPaisDestino     write FPaisDestino;
    property ValorDeclarado: Double   read FValorDeclarado  write FValorDeclarado;

    property UrlConsulta: string      read FUrlConsulta     write FUrlConsulta   stored UrlConsultaAlterada;

    property Itens: TACBrFrenetItens  read FItens           write SetItens       stored TemItens;
  end;



implementation

uses Math;

{ EACBrFrenetException }

constructor EACBrFrenetException.CreateACBrStr(const msg: string);
begin
  inherited Create( ACBrStr(msg) );
end;

{ TACBrFrenet }

constructor TACBrFrenet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FUrlConsulta := CURL_FRENET;
  FValorDeclarado := 0;
  FToken := '';
  FPaisDestino := 'BR';
  FCepDestino := '';
  FCepOrigem := '';

  FErro := 0;
  FMsgErro := '';

  // Json obj
  FJson := TJsonObject.Create;

  // Itens
  FItens := TACBrFrenetItens.Create(True);

  // Retorno das cotações
  FCotacoes := TACBrFrenetCotacoes.Create(True);

  // JSON
  HTTPSend.MimeType := 'application/json';
end;

destructor TACBrFrenet.Destroy;
begin
  FJson.Free;
  FItens.Free;
  FCotacoes.Free;

  inherited Destroy;
end;

function TACBrFrenet.CotarFrete: boolean;
var
  sSend: AnsiString;
  i: integer;
  JItem: TJsonObject;
begin

  try
    FMsgErro := '';

    // Limpa cotações anteriores
    Cotacoes.Clear;

    // Token
    HTTPSend.Headers.Clear;
    HTTPSend.Headers.Add('token: '+ FToken);


    // Montar string json
    Json.Clear;

    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     Json['SellerCEP'].Value := OnlyNumber( FCepOrigem );
     Json['RecipientCEP'].Value := OnlyNumber( FCepDestino );
     Json['ShipmentInvoiceValue'].FloatValue := FValorDeclarado;

     for i := 0 to Itens.Count-1 do
     begin
       // Item
       JItem := Json.A['ShippingItemArray'].AddObject;
       JItem['Weight'].FloatValue := Itens[i].Peso;
       JItem['Length'].FloatValue := Itens[i].Comprimento;
       JItem['Height'].FloatValue := Itens[i].Altura;
       JItem['Width'].FloatValue := Itens[i].Largura;
       JItem['Diameter'].FloatValue := Itens[i].Diametro;
       JItem['Quantity'].IntValue := Max(Itens[i].Quantidade, 1);

       if Itens[i].SKU > '' then
         JItem['SKU'].Value := Itens[i].SKU;

       if Itens[i].Categoria > '' then
         JItem['Category'].Value := Itens[i].Categoria;
     end;

     Json['RecipientCountry'].Value := PaisDestino;

     sSend := Json.ToString;
    {$Else}
     Json['SellerCEP'].AsString := OnlyNumber( FCepOrigem );
     Json['RecipientCEP'].AsString := OnlyNumber( FCepDestino );
     Json['ShipmentInvoiceValue'].AsNumber := FValorDeclarado;

     for i := 0 to Itens.Count-1 do
     begin
       // Item
       Json['ShippingItemArray'].AsArray.Add;
       JItem := Json['ShippingItemArray'].AsArray[ i ].AsObject;
       JItem['Weight'].AsNumber := Itens[i].Peso;
       JItem['Length'].AsNumber := Itens[i].Comprimento;
       JItem['Height'].AsNumber := Itens[i].Altura;
       JItem['Width'].AsNumber := Itens[i].Largura;
       JItem['Diameter'].AsNumber := Itens[i].Diametro;
       JItem['Quantity'].AsInteger := Max(Itens[i].Quantidade, 1);

       if Itens[i].SKU > '' then
         JItem['SKU'].AsString := Itens[i].SKU;

       if Itens[i].Categoria > '' then
         JItem['Category'].AsString := Itens[i].Categoria;
     end;

     Json['RecipientCountry'].AsString := PaisDestino;

     // Converter para UTF-8
     sSend := ACBrAnsiToUTF8(Json.Stringify);
    {$EndIf}

    // Limpar Document
    HTTPSend.Document.Clear;

    // Post
    HTTPSend.Document.Write(Pointer(sSend)^,Length(sSend));

    try
      //DEBUG
      //HTTPSend.Document.SaveToFile('C:\TEMP\CALL_FRENET.HTML');

      HTTPPost(FUrlConsulta + CURL_FRENET_QUOTE);
    except
      on E: Exception do
      begin
        raise EACBrFrenetException.Create('Erro ao consultar Frenet:' + sLineBreak + E.Message);
      end;
    end;

    //DEBUG
    // RespHTTP.SaveToFile('C:\TEMP\FRENET_RESP.HTML');

    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     // Json de retorno
     FreeAndNil(FJson);
     FJson := TJsonObject.Parse(DecodeToString(HTTPResponse, RespIsUTF8)) as TJsonObject;
     // Se não retorno Message, OK
     Result := (Json['message'].Value = '');
    {$Else}
     // Json de retorno
     Json.Parse(DecodeToString(HTTPResponse, RespIsUTF8));
     // Se não retorno Message, OK
     Result := (Json['message'].AsString = '');
    {$EndIf}

    if Result then
      LerCotacoes
    else
      LerMsgErro;

  finally

  end;
end;


function TACBrFrenet.ServicosDisponiveis: boolean;
begin

  try
    FMsgErro := '';

    // Limpa cotações anteriores
    Cotacoes.Clear;

    // Token
    HTTPSend.Headers.Clear;
    HTTPSend.Headers.Add('token: '+ FToken);

    // Chamar o endpoint para ler os serviços disponiveis - GET
    HTTPSend.Document.Clear;

    try

      HTTPMethod('GET', FUrlConsulta + CURL_FRENET_INFO);

    except
      on E: Exception do
      begin
        raise EACBrFrenetException.Create('Erro ao consultar Frenet:' + sLineBreak + E.Message);
      end;
    end;

    // DEBUG
    // RespHTTP.SaveToFile('C:\TEMP\FRENET_RESP.HTML');

    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     // Json de retorno
     FreeAndNil(FJson);
     FJson := TJsonObject.Parse(DecodeToString(HTTPResponse, RespIsUTF8)) as TJsonObject;
     // Se não retorno Message, OK
     Result := (Json['message'].Value = '');
    {$Else}
     // Json de retorno
     Json.Parse(DecodeToString(HTTPResponse, RespIsUTF8));
     // Se não retorno Message, OK
     Result := (Json['message'].AsString = '');
    {$EndIf}

    if Result then
      LerServicos
    else
      LerMsgErro;

  finally

  end;

end;

procedure TACBrFrenet.SetItens(const Value: TACBrFrenetItens);
begin
  FItens.Assign(Value);
end;

function TACBrFrenet.TemItens: Boolean;
begin
  Result := (FItens.Count > 0);
end;

function TACBrFrenet.UrlConsultaAlterada: Boolean;
begin
  Result := (FUrlConsulta <> CURL_FRENET);
end;

procedure TACBrFrenet.LerMsgErro;
begin
  try
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     FMsgErro := Json['message'].Value;
    {$Else}
     FMsgErro := Json['message'].AsString;
    {$EndIf}
  finally

  end;
end;

procedure TACBrFrenet.LerServicos;
var
  i: integer;
  JArray: TJsonArray;
begin

  try
    // Salva o retorno em Cotacoes

    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     JArray := Json['ShippingSeviceAvailableArray'].ArrayValue;

     for i := 0 to JArray.Count-1 do
     begin

       with Cotacoes.New do
       begin
        // So retorna transportador e serviço
         Transportador := JArray[i].ObjectValue['carrier'].Value;
         CodigoTransportador := JArray[i].ObjectValue['CarrierCode'].Value;
         CodigoServico := JArray[i].ObjectValue['ServiceCode'].Value;
         DescricaoServico := JArray[i].ObjectValue['ServiceDescription'].Value;
       end;
     end;
    {$Else}
     JArray := Json['ShippingSeviceAvailableArray'].AsArray;

     for i := 0 to JArray.Count-1 do
     begin

       with Cotacoes.New do
       begin
         // So retorna transportador e serviço
         Transportador := JArray[i].AsObject['carrier'].AsString;
         CodigoTransportador := JArray[i].AsObject['CarrierCode'].AsString;
         CodigoServico := JArray[i].AsObject['ServiceCode'].AsString;
         DescricaoServico := JArray[i].AsObject['ServiceDescription'].AsString;
       end;
     end;
    {$EndIf}
  finally
  end;

end;

{ TACBrFrenetItens }

procedure TACBrFrenetItens.Insert(Index: integer; AObject: TACBrFrenetItem);
begin
  inherited Insert(Index, AObject);
end;

function TACBrFrenetItens.GetItem(Index: integer): TACBrFrenetItem;
begin
  Result := (inherited GetItem(Index) as TACBrFrenetItem);
end;

function TACBrFrenetItens.New: TACBrFrenetItem;
begin
  Result := TACBrFrenetItem.Create;

  Add(Result);
end;

procedure TACBrFrenetItens.SetItem(Index: integer;
  Item: TACBrFrenetItem);
begin
  inherited SetItem(Index, Item);
end;


function TACBrFrenetItens.Add(AObject: TACBrFrenetItem): integer;
begin
  Result := inherited Add(AObject);
end;

{ TACBrFrenetCotacoes }

function TACBrFrenetCotacoes.Add(Obj: TACBrFrenetCotacao): integer;
begin
  Result := inherited Add(Obj);
end;

function TACBrFrenetCotacoes.GetObject(Index: integer): TACBrFrenetCotacao;
begin
  Result := inherited GetItem(Index) as TACBrFrenetCotacao;
end;


function TACBrFrenetCotacoes.New: TACBrFrenetCotacao;
begin
  Result := TACBrFrenetCotacao.Create;

  Add(Result);
end;

procedure TACBrFrenetCotacoes.SetObject(Index: integer;
  Item: TACBrFrenetCotacao);
begin
  inherited SetItem(Index, Item);
end;

procedure TACBrFrenet.LerCotacoes;
var
  i: integer;
  JArray: TJsonArray;
begin
  try
    {$IfDef USE_JSONDATAOBJECTS_UNIT}
     JArray := Json['ShippingSevicesArray'].ArrayValue;

     for i := 0 to JArray.Count-1 do
     begin
       with Cotacoes.New do
       begin

         // Propriedades novas
         // OriginalDeliveryTime = Prazo original, sem modificações de regras de frete
         // ResponseTime = Tempo resposta (ms)
         // OriginalShippingPrice = Valor original, sem modificações de regras de frete

         Transportador := JArray[i].ObjectValue['carrier'].Value;
         CodigoTransportador := JArray[i].ObjectValue['CarrierCode'].Value;
         PrazoEntrega := JArray[i].ObjectValue['DeliveryTime'].IntValue;
         PrazoEntregaOriginal := JArray[i].ObjectValue['OriginalDeliveryTime'].IntValue;
         Mensagem := JArray[i].ObjectValue['msg'].Value;
         CodigoServico := JArray[i].ObjectValue['ServiceCode'].Value;
         DescricaoServico := JArray[i].ObjectValue['ServiceDescription'].Value;

         // Novas propriedades
         TempoResposta := JArray[i].ObjectValue['ResponseTime'].FloatValue;

         // Erro nesta conexão
         Erro := JArray[i].ObjectValue['Error'].BoolValue;

         if Erro then
         begin
           Valor := 0;
           ValorOriginal := 0;
         end
         else
         begin
           Valor :=  JArray[i].ObjectValue['ShippingPrice'].FloatValue;
           ValorOriginal :=  JArray[i].ObjectValue['OriginalShippingPrice'].FloatValue;
         end;
       end;
     end;
    {$Else}
     JArray := Json['ShippingSevicesArray'].AsArray;

     for i := 0 to JArray.Count-1 do
     begin
       with Cotacoes.New do
       begin

         // Propriedades novas
         // OriginalDeliveryTime = Prazo original, sem modificações de regras de frete
         // ResponseTime = Tempo resposta (ms)
         // OriginalShippingPrice = Valor original, sem modificações de regras de frete

         Transportador := JArray[i].AsObject['carrier'].AsString;
         CodigoTransportador := JArray[i].AsObject['CarrierCode'].AsString;
         PrazoEntrega := JArray[i].AsObject['DeliveryTime'].AsInteger;
         PrazoEntregaOriginal := JArray[i].AsObject['OriginalDeliveryTime'].AsInteger;
         Mensagem := JArray[i].AsObject['msg'].AsString;
         CodigoServico := JArray[i].AsObject['ServiceCode'].AsString;
         DescricaoServico := JArray[i].AsObject['ServiceDescription'].AsString;

         // Novas propriedades
         TempoResposta := JArray[i].AsObject['ResponseTime'].AsNumber;

         // Erro nesta conexão
         Erro := JArray[i].AsObject['Error'].AsBoolean;

         if Erro then
         begin
           Valor := 0;
           ValorOriginal := 0;
         end
         else
         begin
           Valor :=  JArray[i].AsObject['ShippingPrice'].AsNumber;
           ValorOriginal :=  JArray[i].AsObject['OriginalShippingPrice'].AsNumber;
         end;
       end;
     end;
    {$EndIf}

  finally

  end;

end;


{ TACBrFrenetItem }

constructor TACBrFrenetItem.Create;
begin
  inherited Create;

  FQuantidade := 1;
  FLargura := 0;
  FAltura := 0;
  FComprimento := 0;
  FDiametro := 0;
  FPeso := 0;
  FSKU := '';
  FCategoria := '';
end;

procedure TACBrFrenetItem.SetAltura(const Value: Double);
begin
  if (FAltura <> Value) then
  begin
    FAltura := Value;
  end;
end;

procedure TACBrFrenetItem.SetCategoria(const Value: string);
begin
  if (FCategoria <> Value) then
  begin
    FCategoria := Value;
  end;
end;

procedure TACBrFrenetItem.SetComprimento(const Value: Double);
begin
  if (FComprimento <> Value) then
  begin
    FComprimento := Value;
  end;
end;

procedure TACBrFrenetItem.SetDiametro(const Value: Double);
begin
  if (FDiametro <> Value) then
  begin
    FDiametro := Value;
  end;
end;

procedure TACBrFrenetItem.SetLargura(const Value: Double);
begin
  if (FLargura <> Value) then
  begin
    FLargura := Value;

  end;
end;

procedure TACBrFrenetItem.SetPeso(const Value: Double);
begin
  if (FPeso <> Value) then
  begin
    FPeso := Value;
  end;
end;

procedure TACBrFrenetItem.SetQuantidade(const Value: integer);
begin
  if (FQuantidade <> Value) then
  begin
    FQuantidade := Value;
  end;
end;

procedure TACBrFrenetItem.SetSKU(const Value: string);
begin
  if (FSKU <> Value) then
  begin
    FSKU := Value;
  end;
end;

end.
