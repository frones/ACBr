{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

{$I ACBr.inc}

unit pcaRetEnvANe;

interface
 uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase,
  pcnConversao, pcaConversao, pcnLeitor;

type
  TRetEnvANe = class;
  TAverbado = class;
  TErros = class;
  TInfos = class;

  TErroCollectionItem = class(TObject)
  private
    FCodigo: String;
    FDescricao: String;
    FValorEsperado: String;
    FValorInformado: String;
  public
    property Codigo: String         read FCodigo         write FCodigo;
    property Descricao: String      read FDescricao      write FDescricao;
    property ValorEsperado: String  read FValorEsperado  write FValorEsperado;
    property ValorInformado: String read FValorInformado write FValorInformado;
  end;

  TErroCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TErroCollectionItem;
    procedure SetItem(Index: Integer; Value: TErroCollectionItem);
  public
    function Add: TErroCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TErroCollectionItem;
    property Items[Index: Integer]: TErroCollectionItem read GetItem write SetItem; default;
  end;

  TErros = class(TObject)
  private
    FErro: TErroCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property Erro: TErroCollection read FErro   write FErro;
  end;

  TDadosSeguroCollectionItem = class(TObject)
  private
    FNumeroAverbacao: String;
    FCNPJSeguradora: String;
    FNomeSeguradora: String;
    FNumApolice: String;
    FTpMov: String;
    FTpDDR: String;
    FValorAverbado: Double;
    FRamoAverbado: String;
  public
    property NumeroAverbacao: String read FNumeroAverbacao write FNumeroAverbacao;
    property CNPJSeguradora: String  read FCNPJSeguradora  write FCNPJSeguradora;
    property NomeSeguradora: String  read FNomeSeguradora  write FNomeSeguradora;
    property NumApolice: String      read FNumApolice      write FNumApolice;
    property TpMov: String           read FTpMov           write FTpMov;
    property TpDDR: String           read FTpDDR           write FTpDDR;
    property ValorAverbado: Double   read FValorAverbado   write FValorAverbado;
    property RamoAverbado: String    read FRamoAverbado    write FRamoAverbado;
  end;

  TDadosSeguroCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDadosSeguroCollectionItem;
    procedure SetItem(Index: Integer; Value: TDadosSeguroCollectionItem);
  public
    function Add: TDadosSeguroCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDadosSeguroCollectionItem;
    property Items[Index: Integer]: TDadosSeguroCollectionItem read GetItem write SetItem; default;
  end;

  TAverbado = class(TObject)
  private
    FdhAverbacao: TDateTime;
    FProtocolo: String;
    FDadosSeguro: TDadosSeguroCollection;
  public
    constructor Create;
    destructor Destroy; override;

    property dhAverbacao: TDateTime read FdhAverbacao write FdhAverbacao;
    property Protocolo: String      read FProtocolo   write FProtocolo;

    property DadosSeguro: TDadosSeguroCollection read FDadosSeguro write FDadosSeguro;
  end;

  TInfoCollectionItem = class(TObject)
  private
    FCodigo: String;
    FDescricao: String;
  public
    property Codigo: String    read FCodigo    write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
  end;

  TInfoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfoCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfoCollectionItem);
  public
    function Add: TInfoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TInfoCollectionItem;
    property Items[Index: Integer]: TInfoCollectionItem read GetItem write SetItem; default;
  end;

  TInfos = class(TObject)
  private
    FInfo: TInfoCollection;
  public
    constructor Create(AOwner: TRetEnvANe);
    destructor Destroy; override;

    property Info: TInfoCollection read FInfo write FInfo;
  end;

  TDeclarado = class(TObject)
  private
    FdhChancela: TDateTime;
    FProtocolo: String;
  public
    property dhChancela: TDateTime read FdhChancela write FdhChancela;
    property Protocolo: String     read FProtocolo  write FProtocolo;
  end;

  TRetEnvANe = class(TObject)
  private
    FLeitor: TLeitor;

    FXML: String;
    FSeguradora: TSeguradora;

    FNumero: String;
    FSerie: String;
    FFilial: String;
    FCNPJCli: String;
    FTpDoc: Integer;
    FInfAdic: String;

    FErros: TErros;
    FAverbado: TAverbado;
    FInfos: TInfos;
    FDeclarado: TDeclarado;

    // ELT
    FCNPJ: String;
    FCTE: String;
    FCodigo: Integer;
    FDataHora: TDateTime;
    FDoc: String;
    FProtocolo: String;
    FResultado: String;
    Ffname: String;
    Fstatus: String;
  public
    constructor Create;
    destructor Destroy; override;

    function LerXml: boolean;
    function LerXml_ATM: boolean;
    function LerXml_ELT: boolean;

    property Leitor: TLeitor read FLeitor write FLeitor;

    property XML: String read FXML write FXML;
    property Seguradora: TSeguradora read FSeguradora write FSeguradora;

    property Numero: String  read FNumero  write FNumero;
    property Serie: String   read FSerie   write FSerie;
    property Filial: String  read FFilial  write FFilial;
    property CNPJCli: String read FCNPJCli write FCNPJCli;
    property TpDoc: Integer  read FTpDoc   write FTpDoc;
    property InfAdic: String read FInfAdic write FInfAdic;

    property Erros: TErros         read FErros     write FErros;
    property Averbado: TAverbado   read FAverbado  write FAverbado;
    property Infos: TInfos         read FInfos     write FInfos;
    property Declarado: TDeclarado read FDeclarado write FDeclarado;

    // ELT
    property CNPJ: String read FCNPJ write FCNPJ;
    property CTE: String read FCTE write FCTE;
    property Codigo: Integer read FCodigo write FCodigo;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property Doc: String read FDoc write FDoc;
    property Protocolo: String read FProtocolo write FProtocolo;
    property Resultado: String read FResultado write FResultado;
    property fname: String read Ffname write Ffname;
    property status: String read Fstatus write Fstatus;
  end;

implementation

{ TErroCollection }

function TErroCollection.Add: TErroCollectionItem;
begin
  Result := Self.New;
end;

function TErroCollection.GetItem(Index: Integer): TErroCollectionItem;
begin
  Result := TErroCollectionItem(inherited Items[Index]);
end;

procedure TErroCollection.SetItem(Index: Integer;
  Value: TErroCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TErro }

constructor TErros.Create;
begin
  inherited Create;
  FErro := TErroCollection.Create;
end;

destructor TErros.Destroy;
begin
  FErro.Free;

  inherited;
end;

function TErroCollection.New: TErroCollectionItem;
begin
  Result := TErroCollectionItem.Create;
  Self.Add(Result);
end;

{ TDadosSeguroCollection }

function TDadosSeguroCollection.Add: TDadosSeguroCollectionItem;
begin
  Result := Self.New;
end;

function TDadosSeguroCollection.GetItem(
  Index: Integer): TDadosSeguroCollectionItem;
begin
  Result := TDadosSeguroCollectionItem(inherited Items[Index]);
end;

procedure TDadosSeguroCollection.SetItem(Index: Integer;
  Value: TDadosSeguroCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDadosSeguroCollection.New: TDadosSeguroCollectionItem;
begin
  Result := TDadosSeguroCollectionItem.Create;
  Self.Add(Result);
end;

{ TAverbado }

constructor TAverbado.Create;
begin
  inherited Create;
  FDadosSeguro := TDadosSeguroCollection.Create;
end;

destructor TAverbado.Destroy;
begin
  FDadosSeguro.Free;

  inherited;
end;

{ TInfoCollection }

function TInfoCollection.Add: TInfoCollectionItem;
begin
  Result := Self.New;
end;

function TInfoCollection.GetItem(Index: Integer): TInfoCollectionItem;
begin
  Result := TInfoCollectionItem(inherited Items[Index]);
end;

procedure TInfoCollection.SetItem(Index: Integer;
  Value: TInfoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfoCollection.New: TInfoCollectionItem;
begin
  Result := TInfoCollectionItem.Create;
  Self.Add(Result);
end;

{ TInfos }

constructor TInfos.Create;
begin
  inherited Create;
  FInfo := TInfoCollection.Create;
end;

destructor TInfos.Destroy;
begin
  FInfo.Free;
  
  inherited;
end;

{ TRetEnvANe }

constructor TRetEnvANe.Create;
begin
  inherited Create;
  FLeitor    := TLeitor.Create;
  FErros     := TErros.Create;
  FAverbado  := TAverbado.Create;
  FInfos     := TInfos.Create( Self );
  FDeclarado := TDeclarado.Create;
end;

destructor TRetEnvANe.Destroy;
begin
  FLeitor.Free;
  FErros.Free;
  FAverbado.Free;
  FInfos.Free;
  FDeclarado.Free;

  inherited;
end;

function TRetEnvANe.LerXml: boolean;
begin
  case Seguradora of
    tsATM: Result := LerXml_ATM;
    tsELT: Result := LerXml_ELT;
  else
    Result := False;
  end;
end;

function TRetEnvANe.LerXml_ATM: boolean;
var
  i: Integer;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;

    FXML := Leitor.Grupo;

    if (leitor.rExtrai(1, 'ns1:averbaCTeResponse') <> '') or
       (leitor.rExtrai(1, 'ns1:averbaNFeResponse') <> '') then
    begin
      if (leitor.rExtrai(2, 'Response') <> '') then
      begin
        FNumero  := Leitor.rCampo(tcStr, 'Numero');
        FSerie   := Leitor.rCampo(tcStr, 'Serie');
        FFilial  := Leitor.rCampo(tcStr, 'Filial');
        FCNPJCli := Leitor.rCampo(tcStr, 'CNPJCli');
        FTpDoc   := Leitor.rCampo(tcInt, 'TpDoc');
        FInfAdic := Leitor.rCampo(tcStr, 'InfAdic');

        if (leitor.rExtrai(3, 'Erros') <> '') then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'Erro', '', i + 1) <> '' do
          begin
            Erros.FErro.New;
            Erros.FErro[i].FCodigo         := Leitor.rCampo(tcStr, 'Codigo');
            Erros.FErro[i].FDescricao      := Leitor.rCampo(tcStr, 'Descricao');
            Erros.FErro[i].FValorEsperado  := Leitor.rCampo(tcStr, 'ValorEsperado');
            Erros.FErro[i].FValorInformado := Leitor.rCampo(tcStr, 'ValorInformado');

            inc(i);
          end;
        end;

        if leitor.rExtrai(3, 'Averbado') <> '' then
        begin
          Averbado.FdhAverbacao := Leitor.rCampo(tcDatHor, 'dhAverbacao');
          Averbado.FProtocolo   := Leitor.rCampo(tcStr, 'Protocolo');

          i := 0;
          while Leitor.rExtrai(4, 'DadosSeguro', '', i + 1) <> '' do
          begin
            Averbado.FDadosSeguro.New;
            Averbado.FDadosSeguro[i].FNumeroAverbacao := Leitor.rCampo(tcStr, 'NumeroAverbacao');
            Averbado.FDadosSeguro[i].FCNPJSeguradora  := Leitor.rCampo(tcStr, 'CNPJSeguradora');
            Averbado.FDadosSeguro[i].FNomeSeguradora  := Leitor.rCampo(tcStr, 'NomeSeguradora');
            Averbado.FDadosSeguro[i].FNumApolice      := Leitor.rCampo(tcStr, 'NumApolice');
            Averbado.FDadosSeguro[i].FTpMov           := Leitor.rCampo(tcStr, 'TpMov');
            Averbado.FDadosSeguro[i].FTpDDR           := Leitor.rCampo(tcStr, 'TpDDR');
            Averbado.FDadosSeguro[i].FValorAverbado   := Leitor.rCampo(tcDe2, 'ValorAverbado');
            Averbado.FDadosSeguro[i].FRamoAverbado    := Leitor.rCampo(tcStr, 'RamoAverbado');

            inc(i);
          end;
        end;

        if leitor.rExtrai(3, 'Infos') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'Info', '', i + 1) <> '' do
          begin
            Infos.FInfo.New;
            Infos.FInfo[i].FCodigo    := Leitor.rCampo(tcStr, 'Codigo');
            Infos.FInfo[i].FDescricao := Leitor.rCampo(tcStr, 'Descricao');

            inc(i);
          end;
        end;

        Result := True;
      end;
    end;

    if leitor.rExtrai(1, 'ns1:declaraMDFeResponse') <> '' then
    begin
      if (leitor.rExtrai(2, 'Response') <> '') then
      begin
        FNumero := Leitor.rCampo(tcStr, 'Numero');
        FSerie  := Leitor.rCampo(tcStr, 'Serie');
        FFilial := Leitor.rCampo(tcStr, 'Filial');

        if (leitor.rExtrai(3, 'Erros') <> '') then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'Erro', '', i + 1) <> '' do
          begin
            Erros.FErro.New;
            Erros.FErro[i].FCodigo         := Leitor.rCampo(tcStr, 'Codigo');
            Erros.FErro[i].FDescricao      := Leitor.rCampo(tcStr, 'Descricao');
            Erros.FErro[i].FValorEsperado  := Leitor.rCampo(tcStr, 'ValorEsperado');
            Erros.FErro[i].FValorInformado := Leitor.rCampo(tcStr, 'ValorInformado');

            inc(i);
          end;
        end;

        if (leitor.rExtrai(3, 'Declarado') <> '') then
        begin
          Declarado.FdhChancela := Leitor.rCampo(tcDatHor, 'dhChancela');
          Declarado.FProtocolo  := Leitor.rCampo(tcStr, 'Protocolo');
        end;

        if leitor.rExtrai(3, 'Infos') <> '' then
        begin
          i := 0;
          while Leitor.rExtrai(4, 'Info', '', i + 1) <> '' do
          begin
            Infos.FInfo.New;
            Infos.FInfo[i].FCodigo    := Leitor.rCampo(tcStr, 'Codigo');
            Infos.FInfo[i].FDescricao := Leitor.rCampo(tcStr, 'Descricao');

            inc(i);
          end;
        end;

        Result := True;
      end;
    end;

  except
    Result := False;
  end;
end;

function TRetEnvANe.LerXml_ELT: boolean;
begin
  Result := False;

  try
    Leitor.Grupo := Leitor.Arquivo;

    FXML := Leitor.Grupo;

    if (leitor.rExtrai(1, 'RespostaProcessamento') <> '') then
    begin
      FCNPJ      := Leitor.rCampo(tcStr, 'CNPJ');
      FCTE       := Leitor.rCampo(tcStr, 'CTE');
      FCodigo    := Leitor.rCampo(tcInt, 'Codigo');
      FDataHora  := Leitor.rCampo(tcDatHor, 'DataHora');
      FDoc       := Leitor.rCampo(tcStr, 'Doc');
      FProtocolo := Leitor.rCampo(tcStr, 'Protocolo');
      FResultado := Leitor.rCampo(tcStr, 'Resultado');
      Ffname     := Leitor.rCampo(tcStr, 'fname');
      Fstatus    := Leitor.rCampo(tcStr, 'status');
    end;
  except
    Result := False;
  end;
end;

end.

