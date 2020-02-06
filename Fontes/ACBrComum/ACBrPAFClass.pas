{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }
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
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrPAFClass;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs
  {$Else}
   Contnrs
  {$IfEnd};

type
  TACBrPAFTipoFuncionamento = (tpfStandAlone, tpfEmRede, tpfParametrizavel);
  TACBrPAFTipoDesenvolvimento = (tpdComercializavel, tpdExclusivoProprio, tpdExclusivoTerceirizado);
  TACBrPAFTipoIntegracao = (tpiRetaguarda, tpiPED, tpiAmbos, tpiNaoIntegra);


  { TACBrAACECF }

  TACBrAACECF = class( TPersistent )
  private
    FCRO : Integer ;
    FDtHrAtualizado : TDateTime ;
    FValorGT: Double;
    FNumeroSerie: String;
    FCNI : Integer;
    function GetLinhaDados : String ;
    procedure SetLinhaDados(const AValue : String) ;
    procedure SetValorGT(const AValue : Double) ;
  public
    property NumeroSerie    : String    read FNumeroSerie    write FNumeroSerie;
    property CRO            : Integer   read FCRO            write FCRO;
    property ValorGT        : Double    read FValorGT        write SetValorGT;
    property DtHrAtualizado : TDateTime read FDtHrAtualizado write FDtHrAtualizado;  
    property CNI            : Integer   read FCNI            write FCNI;            //Codigo Nacional de Identificacao do ECF

    property LinhaDados : String read GetLinhaDados write SetLinhaDados ;
  end;

  { TACBrAACECFs }

  TACBrAACECFs = class(TObjectList{$IfDef NEXTGEN}<TACBrAACECF>{$EndIf})
  private
    procedure SetObject(Index: Integer; Item: TACBrAACECF);
    function GetObject(Index: Integer): TACBrAACECF;
  public
    function New: TACBrAACECF;
    function Add(Obj: TACBrAACECF): Integer;
    procedure Insert(Index: Integer; Obj: TACBrAACECF);
    property Objects[Index: Integer]: TACBrAACECF read GetObject write SetObject; default;
  end;

  { TACBrECFEmpresa }

  TACBrECFEmpresa = class( TPersistent )
  private
    fsCNPJ: string;
    fsUf: String;
    fsCep: String;
    fsRazaoSocial: string;
    fsContato: string;
    fsCidade: String;
    fsEndereco: string;
    fsTelefone: string;
    fsEmail: String;
    fsIE: String;
    fsIM: String;
    procedure SetCNPJ(const AValue : string) ;
    procedure SetIE(const AValue : String) ;
    procedure SetIM(const AValue : String) ;
    procedure SetRazaoSocial(const AValue : string) ;
  public
  published
    property CNPJ: string read fsCNPJ write SetCNPJ;
    property RazaoSocial: string read fsRazaoSocial write SetRazaoSocial;
    property Endereco: string read fsEndereco write fsEndereco;
    property Cep: String read fsCep write fsCep;
    property Cidade: String read fsCidade write fsCidade;
    property Uf: String read fsUf write fsUf;
    property Telefone: string read fsTelefone write fsTelefone;
    property Contato: string read fsContato write fsContato;
    property Email: String read fsEmail write fsEmail;
    property IE: String read fsIE write SetIE;
    property IM: String read fsIM write SetIM;
  end;

  { TACBrECFArquivo }

  TACBrECFArquivo = class( TPersistent )
  private
    fsMD5: string;
    fsNome: String;
    procedure SetMD5(const AValue : string) ;
    procedure SetNome(const AValue : String) ;
  published
    property Nome: String read fsNome write SetNome;
    property MD5: string read fsMD5 write SetMD5;
  end;

  { TACBrECFArquivos }

  TACBrECFArquivos = class(TObjectList{$IfDef NEXTGEN}<TACBrECFArquivo>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFArquivo);
    function GetObject (Index: Integer): TACBrECFArquivo;
    procedure Insert (Index: Integer; Obj: TACBrECFArquivo);
  public
    function New: TACBrECFArquivo;
    function Add (Obj: TACBrECFArquivo): Integer; overload ;
    function Add (const Nome: String): Integer; overload ;
    property Objects [Index: Integer]: TACBrECFArquivo
      read GetObject write SetObject; default;
  end;

  { TACBrECFInfoPaf }

  TACBrECFInfoPaf = class( TPersistent )
  private
    FRecompoeNumSerie : Boolean ;
    fsVersao: String;
    fsPrincipalExe: TACBrECFArquivo;
    fsNome: String;
    FDAVDiscrFormula: Boolean;
    FBarSimilarECFRestaurante: Boolean;
    FRealizaDAVECF: Boolean;
    FTransfPreVenda: Boolean;
    FBarSimilarECFComum: Boolean;
    FBarSimilarBalanca: Boolean;
    fUFContribuinte: String;
    FUsaImpressoraNaoFiscal: Boolean;
    FTransportePassageiro: Boolean;
    FRealizaDAVNaoFiscal: Boolean;
    FIndiceTecnicoProd: Boolean;
    FRealizaPreVenda: Boolean;
    FTransfDAV: Boolean;
    FTotalizaValoresLista: Boolean;
    FDAVConfAnexoII: Boolean;
    FRealizaLancamentoMesa: Boolean;
    FTipoDesenvolvimento: TACBrPAFTipoDesenvolvimento;
    FTipoFuncionamento: TACBrPAFTipoFuncionamento;
    FImpedeVendaVlrZero: Boolean;
    FAcumulaVolumeDiario: Boolean;
    FArmazenaEncerranteIniFinal: Boolean;
    FEmiteContrEncerrAposREDZLEIX: Boolean;
    FIntegradoComBombas: Boolean;
    FCriaAbastDivergEncerrante: Boolean;
    FCadastroPlacaBomba: Boolean;
    FRealizaDAVOS: Boolean;
    FIntegracaoPAFECF: TACBrPAFTipoIntegracao;
    FRecompoeGT: Boolean;
    FEmitePED: Boolean;
    FMinasLegal: Boolean;
    FCupomMania: Boolean;
    fsBancoDados: String;
    fsSistemaOperacional: String;
    fsLinguagem: String;
    FTrocoEmCartao: Boolean;
    FNotaLegalDF: Boolean;
    FParaibaLegal: Boolean;
    FPerfilRequisitos: String;
    procedure SetVersao(const AValue : String) ;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Nome: String read fsNome write fsNome;
    property Versao: String read fsVersao write SetVersao;
    property BancoDados: String read fsBancoDados write fsBancoDados;
    property Linguagem: String read fsLinguagem write fsLinguagem;
    property SistemaOperacional: String read fsSistemaOperacional write fsSistemaOperacional;
    property PrincipalExe: TACBrECFArquivo read fsPrincipalExe write fsPrincipalExe;

    //funcionalidades
    property TipoFuncionamento: TACBrPAFTipoFuncionamento read FTipoFuncionamento write FTipoFuncionamento;
    property TipoDesenvolvimento: TACBrPAFTipoDesenvolvimento read FTipoDesenvolvimento write FTipoDesenvolvimento;
    property IntegracaoPAFECF: TACBrPAFTipoIntegracao read FIntegracaoPAFECF write FIntegracaoPAFECF;

    //parametros de nao concomitancia
    property RealizaPreVenda: Boolean read FRealizaPreVenda write FRealizaPreVenda;
    property RealizaDAVECF: Boolean read FRealizaDAVECF write FRealizaDAVECF;
    property RealizaDAVNaoFiscal: Boolean read FRealizaDAVNaoFiscal write FRealizaDAVNaoFiscal;
    property RealizaDAVOS: Boolean read FRealizaDAVOS write FRealizaDAVOS;
    property DAVConfAnexoII: Boolean read FDAVConfAnexoII write FDAVConfAnexoII;
    property RealizaLancamentoMesa: Boolean read FRealizaLancamentoMesa write FRealizaLancamentoMesa;

   // aplicações especiais
    property IndiceTecnicoProd: Boolean read FIndiceTecnicoProd write FIndiceTecnicoProd;
    property BarSimilarECFRestaurante: Boolean read FBarSimilarECFRestaurante write FBarSimilarECFRestaurante;
    property BarSimilarECFComum: Boolean read FBarSimilarECFComum write FBarSimilarECFComum;
    property BarSimilarBalanca: Boolean read FBarSimilarBalanca write FBarSimilarBalanca;
    property UsaImpressoraNaoFiscal: Boolean read FUsaImpressoraNaoFiscal write FUsaImpressoraNaoFiscal;
    property DAVDiscrFormula: Boolean read FDAVDiscrFormula write FDAVDiscrFormula;

    //posto
    property ImpedeVendaVlrZero: Boolean read FImpedeVendaVlrZero write FImpedeVendaVlrZero;
    property AcumulaVolumeDiario: Boolean read FAcumulaVolumeDiario write FAcumulaVolumeDiario;
    property ArmazenaEncerranteIniFinal: Boolean read FArmazenaEncerranteIniFinal write FArmazenaEncerranteIniFinal;
    property EmiteContrEncerrAposREDZLEIX: Boolean read FEmiteContrEncerrAposREDZLEIX write FEmiteContrEncerrAposREDZLEIX;
    property IntegradoComBombas:Boolean read FIntegradoComBombas write FIntegradoComBombas;
    property CriaAbastDivergEncerrante: Boolean read FCriaAbastDivergEncerrante write FCriaAbastDivergEncerrante;
    property CadastroPlacaBomba: Boolean read FCadastroPlacaBomba write FCadastroPlacaBomba;

    // transporte de passageiros
    property TransportePassageiro: Boolean read FTransportePassageiro write FTransportePassageiro;

    //criterios por uf
    property TotalizaValoresLista: Boolean read FTotalizaValoresLista write FTotalizaValoresLista;
    property TransfPreVenda: Boolean read FTransfPreVenda write FTransfPreVenda;
    property TransfDAV: Boolean read FTransfDAV write FTransfDAV;
    property RecompoeGT: Boolean read FRecompoeGT write FRecompoeGT;
    property RecompoeNumSerie: Boolean read FRecompoeNumSerie write FRecompoeNumSerie;
    property EmitePED: Boolean read FEmitePED write FEmitePED;
    property CupomMania: Boolean read FCupomMania write FCupomMania;
    property MinasLegal: Boolean read FMinasLegal write FMinasLegal;
    property NotaLegalDF: Boolean read FNotaLegalDF write FNotaLegalDF;
    property ParaibaLegal: Boolean read FParaibaLegal write FParaibaLegal;
    property TrocoEmCartao: Boolean read FTrocoEmCartao write FTrocoEmCartao;

    // homologação 02.01
    property PerfilRequisitos: String read FPerfilRequisitos write FPerfilRequisitos;

    // homologação 02.04
    property UFContribuinte: String read fUFContribuinte write fUFContribuinte;
  end;

  TACBrECFIdentificacaoPAF = class( TPersistent )
  private
    fsNumeroLaudo: String;
    fsNumeroCredencimento : string;
    fsEmpresa: TACBrECFEmpresa;
    fsPaf: TACBrECFInfoPaf;
    fsOutrosArquivos: TACBrECFArquivos;
    fsECFsAutorizados: TACBrAACECFs;
    fsArquivoListaAutenticados: TACBrECFArquivo;
    fsVersaoER: String;
    fsDataLaudo: TDateTime;
  public
    constructor Create ;
    destructor Destroy; override;
    property ECFsAutorizados: TACBrAACECFs read fsECFsAutorizados write fsECFsAutorizados;
    property OutrosArquivos: TACBrECFArquivos read fsOutrosArquivos write fsOutrosArquivos;
    property ArquivoListaAutenticados: TACBrECFArquivo read fsArquivoListaAutenticados write fsArquivoListaAutenticados;
  published
    property NumeroLaudo: String read fsNumeroLaudo write fsNumeroLaudo;
    property DataLaudo: TDateTime read fsDataLaudo write fsDataLaudo;
    property NumeroCredencimento : string read fsNumeroCredencimento write fsNumeroCredencimento;
    property VersaoER: String read fsVersaoER write fsVersaoER;
    property Empresa: TACBrECFEmpresa read fsEmpresa write fsEmpresa;
    property Paf: TACBrECFInfoPaf read fsPaf write fsPaf;
  end;

  { Definindo novo tipo para armazenar DAV }
  TACBrECFDAV = class
  private
    fsTitulo: String;
    fsValor: Double;
    fsCOO_Cupom: integer;
    fsCOO_Dav: integer;
    fsNumero: String;
    fsDtEmissao: TDateTime;
  public
    constructor Create;
    procedure Assign(ADAV: TACBrECFDAV);

    property Numero: String read fsNumero write fsNumero;
    property Titulo: String read fsTitulo write fsTitulo;
    property COO_Dav: integer read fsCOO_Dav write fsCOO_Dav;
    property COO_Cupom: integer read fsCOO_Cupom write fsCOO_Cupom;
    property DtEmissao: TDateTime read fsDtEmissao write fsDtEmissao;
    property Valor: Double read fsValor write fsValor;
  end;

  { Lista de Objetos do tipo TACBrECFDAV }
  TACBrECFDAVs = class(TObjectList{$IfDef NEXTGEN}<TACBrECFDAV>{$EndIf})
  private
    procedure SetObject (Index: Integer; Item: TACBrECFDAV);
    function GetObject (Index: Integer): TACBrECFDAV;
  public
    procedure Ordenar;
    function New: TACBrECFDAV;
    function Add (Obj: TACBrECFDAV): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFDAV);
    function ValorTotalAcumulado: Double;  protected
    property Objects [Index: Integer]: TACBrECFDAV
      read GetObject write SetObject; default;
  end;

implementation

Uses ACBrUtil, strutils, math ;

{ TACBrECFEmpresa }

procedure TACBrECFEmpresa.SetCNPJ(const AValue : string) ;
begin
  fsCNPJ := Trim(LeftStr( OnlyNumber( AValue ), 14));
end ;

procedure TACBrECFEmpresa.SetIE(const AValue : String) ;
begin
  if AValue <> 'ISENTO' then
     fsIE := Trim(LeftStr( OnlyNumber( AValue ), 14))
  else
     fsIE := AValue;
end ;

procedure TACBrECFEmpresa.SetIM(const AValue : String) ;
begin
  fsIM := Trim(LeftStr( OnlyNumber( AValue ), 14));
end ;

procedure TACBrECFEmpresa.SetRazaoSocial(const AValue : string) ;
begin
  fsRazaoSocial := AValue;
end ;

{ TACBrECFArquivo }

procedure TACBrECFArquivo.SetMD5(const AValue : string) ;
begin
   fsMD5 := UpperCase(Trim(LeftStr( AValue, 32)));
end ;

procedure TACBrECFArquivo.SetNome(const AValue : String) ;
begin
  fsNome := Trim(LeftStr( AValue, 40));
end ;

{ TACBrECFArquivos }

function TACBrECFArquivos.Add(Obj: TACBrECFArquivo): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TACBrECFArquivos.Add(const Nome : String) : Integer ;
var
   ACBrECFArquivo : TACBrECFArquivo ;
begin
  ACBrECFArquivo := TACBrECFArquivo.Create;
  ACBrECFArquivo.Nome := Nome;
  Result := inherited Add(ACBrECFArquivo) ;
end ;

function TACBrECFArquivos.GetObject(Index: Integer): TACBrECFArquivo;
begin
  Result := TACBrECFArquivo(inherited Items[Index]);
end;

procedure TACBrECFArquivos.Insert(Index: Integer; Obj: TACBrECFArquivo);
begin
  inherited Insert(Index, Obj);
end;

function TACBrECFArquivos.New: TACBrECFArquivo;
begin
  Result := TACBrECFArquivo.Create;
  Add(Result);
end;

procedure TACBrECFArquivos.SetObject(Index: Integer; Item: TACBrECFArquivo);
begin
  inherited Items[Index] := Item;
end;

{ TACBrECFInfoPaf }

procedure TACBrECFInfoPaf.SetVersao(const AValue : String) ;
begin
  fsVersao := Trim(LeftStr( AValue, 10));
end ;

constructor TACBrECFInfoPaf.Create;
begin
  inherited;
  fsPrincipalExe := TACBrECFArquivo.Create;
end;

destructor TACBrECFInfoPaf.Destroy;
begin
  fsPrincipalExe.Free;
  inherited;
end;

{ TACBrECFIdentificacaoPAF }

constructor TACBrECFIdentificacaoPAF.Create ;
begin
  inherited ;
  fsPaf := TACBrECFInfoPaf.Create;
  fsEmpresa := TACBrECFEmpresa.Create;
  fsOutrosArquivos := TACBrECFArquivos.Create;
  fsECFsAutorizados := TACBrAACECFs.Create;
  fsArquivoListaAutenticados := TACBrECFArquivo.Create;
end;

destructor TACBrECFIdentificacaoPAF.Destroy;
begin
  fsPaf.Free;
  fsEmpresa.Free;
  fsOutrosArquivos.Free;
  fsECFsAutorizados.Free;
  fsArquivoListaAutenticados.Free;

  inherited;
end;

{ TACBrAACECF }

function TACBrAACECF.GetLinhaDados : String ;
begin
  Result := NumeroSerie           + '|' +
            IntToStr(CRO)         + '|' +
            FloatToStr(ValorGT)   + '|' +
            DTtoS(DtHrAtualizado) + '|' +
            IntToStr(CNI)         ;
end ;

procedure TACBrAACECF.SetLinhaDados(const AValue : String) ;
var
   SL : TStringList ;
begin
  SL := TStringList.Create;
  try
     SL.Text := StringReplace( AValue, '|', sLineBreak, [rfReplaceAll] ) ;

     if SL.Count < 4 then exit ;

     NumeroSerie    := SL[0] ;
     CRO            := StrToIntDef( SL[1], 0) ;
     ValorGT        := StringToFloatDef( SL[2], 0) ;
     DtHrAtualizado := StoD( SL[3] ) ;  
     if SL.Count > 4 then  
        CNI := StrToIntDef( SL[4], 0)
     else
        CNI := 0 ;
  finally
     SL.Free;
  end ;
end ;

procedure TACBrAACECF.SetValorGT(const AValue : Double) ;
begin
   FValorGT := RoundTo( AValue, -2) ;
end ;

{ TACBrAACECFs }

function TACBrAACECFs.Add(Obj: TACBrAACECF): Integer;
begin
  Result := inherited Add(Obj);
end;

function TACBrAACECFs.GetObject( Index: Integer): TACBrAACECF;
begin
  Result := TACBrAACECF(inherited Items[Index]);
end;

procedure TACBrAACECFs.Insert(Index: Integer; Obj: TACBrAACECF);
begin
  inherited Insert(Index, Obj);
end;

function TACBrAACECFs.New : TACBrAACECF;
begin
  Result := TACBrAACECF.Create;
  Add(Result);
end;

procedure TACBrAACECFs.SetObject(Index: Integer; Item: TACBrAACECF);
begin
  inherited Items[Index] := Item;
end;

{ ---------------------------- TACBrECFDAVs -------------------------- }

{ TACBrECFDAV }

function OrdenarDAVs(const ADav1, ADav2: {$IfDef NEXTGEN}TACBrECFDAV{$Else}Pointer{$EndIf}): Integer;
var
  Str1, Str2 : String ;
begin
  with TACBrECFDAV(ADav1) do
     Str1 := DtoS( DtEmissao ) + Trim(Numero) ;

  with TACBrECFDAV(ADav2) do
     Str2 := DtoS( DtEmissao ) + Trim(Numero) ;

  if Str1 < Str2 then
    Result := -1
  else if Str1 > Str2 then
    Result := 1
  else
    Result := 0;
end;

constructor TACBrECFDAV.create;
begin
  fsNumero    := EmptyStr;
  fsCOO_Cupom := 0;
  fsCOO_Dav   := 0;
  fsTitulo    := '';
  fsValor     := 0.00;
  fsDtEmissao := 0.0;
end;

procedure TACBrECFDAV.Assign(ADAV: TACBrECFDAV);
begin
  fsNumero    := ADAV.Numero;
  fsTitulo    := ADAV.Titulo;
  fsValor     := ADAV.Valor;
  fsDtEmissao := ADAV.DtEmissao;
  fsCOO_Dav   := ADAV.COO_Dav;
  fsCOO_Cupom := ADAV.COO_Cupom;
end;

function TACBrECFDAVs.Add(Obj: TACBrECFDAV): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TACBrECFDAVs.GetObject(Index: Integer): TACBrECFDAV;
begin
  Result := TACBrECFDAV(inherited Items[Index]);
end;

procedure TACBrECFDAVs.Insert(Index: Integer; Obj: TACBrECFDAV);
begin
  inherited Insert(Index, Obj);
end;

function TACBrECFDAVs.New: TACBrECFDAV;
begin
  Result := TACBrECFDAV.create;
  Add(Result);
end;

procedure TACBrECFDAVs.Ordenar;
begin
  {$IfDef NEXTGEN}
  Self.Sort( TComparer<TACBrECFDAV>.Construct( OrdenarDAVs ) );
  {$Else}
  Self.Sort(@OrdenarDAVs);
  {$EndIf}
end;

function TACBrECFDAVs.ValorTotalAcumulado: Double;
var
  I: Integer;
begin
  Result := 0.00;
  for I := 0 to Self.Count - 1 do
    Result := Result + Self[I].Valor;
end;

procedure TACBrECFDAVs.SetObject(Index: Integer; Item: TACBrECFDAV);
begin
  inherited Items[Index] := Item;
end;


end.
