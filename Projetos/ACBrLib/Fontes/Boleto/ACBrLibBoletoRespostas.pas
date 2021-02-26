{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: José M S Junior                                  }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibBoletoRespostas;

interface

uses
  SysUtils, Classes,
  ACBrLibResposta, ACBrBoleto, contnrs;

type

  { TLibBoletoServiceResposta }
  TLibBoletoServiceResposta = class abstract(TACBrLibResposta<TACBrBoleto>)
  private

  public
    constructor Create(const ASessao: String; const ATipo: TACBrLibRespostaTipo;
      const AFormato: TACBrLibCodificacao); reintroduce;

    procedure Processar(const ACBrBoleto: TACBrBoleto); virtual; abstract; reintroduce;

  end;

  { TRetornoDadosCedente }
  TRetornoDadosCedente = class(TLibBoletoServiceResposta)
  private
    FNome: String;
    FCNPJCPF: String;
    FCodigoCedente: String;
    FModalidade: String;
    FCodTransmissao: String;
    FConvenio:String;
  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property Nome : String read FNome Write FNome;
    property CNPJCPF : String read FCNPJCPF Write FCNPJCPF;
    property CodigoCedente : String read FCodigoCedente Write FCodigoCedente;
    property Modalidade : String read FModalidade Write FModalidade;
    property CodTransmissao : String read FCodTransmissao Write FCodTransmissao;
    property Convenio : String read FConvenio Write FConvenio;

  end;

  { TRetornoDadosBanco }
  TRetornoDadosBanco = class(TLibBoletoServiceResposta)
  private
    FNumero : Integer;
    FIndiceACBr : Integer;
    FNumeroCorrespondente : Integer;
    FVersaoArquivo : Integer;
    FVersaoLote : Integer;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property Numero : Integer read FNumero write FNumero;
    property IndiceACBr : Integer read FIndiceACBr write FIndiceACBr ;
    property NumeroCorrespondente : Integer read FNumeroCorrespondente write FNumeroCorrespondente;
    property VersaoArquivo : Integer read FVersaoArquivo write FVersaoArquivo;
    property VersaoLote : Integer read FVersaoLote write FVersaoLote;

  end;

  { TRetornoDadosConta }
  TRetornoDadosConta = class(TLibBoletoServiceResposta)
  private
    FConta : String;
    FDigitoConta : String;
    FAgencia : String;
    FDigitoAgencia : String;
    FDigitoVerificadorAgenciaConta : String;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property Conta : String read FConta write FConta;
    property DigitoConta : String read FDigitoConta write FDigitoConta;
    property Agencia : String read FAgencia write FAgencia;
    property DigitoAgencia : String read FDigitoAgencia write FDigitoAgencia;
    property DigitoVerificadorAgenciaConta : String read FDigitoVerificadorAgenciaConta write FDigitoVerificadorAgenciaConta;

  end;

   { TRetornoRejeicoesTitulo }
  TRetornoRejeicoesTitulo = class(TACBrLibRespostaBase)
  private
    FID : Integer;
    FIDRej : Integer;
    FMotivoRejeicao : String;

  public
    constructor Create( const AIDRej: Integer; const AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
    procedure Processar(const ACBrBoleto: TACBrBoleto);

  published
    property MotivoRejeicao : String read FMotivoRejeicao write FMotivoRejeicao;

  end;

  { TRetornoDadosTitulo }
  TRetornoDadosTitulo = class(TLibBoletoServiceResposta)
  private
    FID: Integer;
    FSacado_Nome : String;
    FSacado_CNPJCPF : String;
    FVencimento: TDateTime;
    FDataDocumento: TDateTime;
    FNumeroDocumento: String;
    FDataProcessamento: TDateTime;
    FNossoNumero: String;
    FCarteira: String;
    FValorDocumento: TDateTime;
    FDataOcorrencia: TDateTime;
    FDataCredito: TDateTime;
    FDataBaixa: TDateTime;
    FDataMoraJuros: TDateTime;
    FValorDespesaCobranca: Currency;
    FValorAbatimento: Currency;
    FValorDesconto: Currency;
    FValorMoraJuros: Currency;
    FValorIOF: Currency;
    FValorOutrasDespesas: Currency;
    FValorOutrosCreditos: Currency;
    FValorRecebido: Currency;
    FSeuNumero: String;
    FCodTipoOcorrencia: String;
    FDescricaoTipoOcorrencia: String;
    FMotivoRejeicao: TRetornoRejeicoesTitulo;

  public
    constructor Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;
    procedure Processar( const ACBrBoleto: TACBrBoleto); override;

  published
    property Sacado_Nome : String read FSacado_Nome write FSacado_Nome;
    property Sacado_CNPJCPF : String read FSacado_CNPJCPF write FSacado_CNPJCPF;
    property Vencimento: TDateTime read FVencimento write FVencimento;
    property DataDocumento: TDateTime read FDataDocumento write FDataDocumento;
    property NumeroDocumento: String read FNumeroDocumento write FNumeroDocumento;
    property DataProcessamento: TDateTime read FDataProcessamento write FDataProcessamento;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property Carteira: String read FCarteira write FCarteira;
    property ValorDocumento: TDateTime read FValorDocumento write FValorDocumento;
    property DataOcorrencia: TDateTime read FDataOcorrencia write FDataOcorrencia;
    property DataCredito: TDateTime read FDataCredito write FDataCredito;
    property DataBaixa: TDateTime read FDataBaixa write FDataBaixa;
    property DataMoraJuros: TDateTime read FDataMoraJuros write FDataMoraJuros;
    property ValorDespesaCobranca: Currency read FValorDespesaCobranca write FValorDespesaCobranca;
    property ValorAbatimento: Currency read FValorAbatimento write FValorAbatimento;
    property ValorDesconto: Currency read FValorDesconto write FValorDesconto;
    property ValorMoraJuros: Currency read FValorMoraJuros write FValorMoraJuros;
    property ValorIOF: Currency read FValorIOF write FValorIOF;
    property ValorOutrasDespesas: Currency read FValorOutrasDespesas write FValorOutrasDespesas;
    property ValorOutrosCreditos: Currency read FValorOutrosCreditos write FValorOutrosCreditos;
    property ValorRecebido: Currency read FValorRecebido write FValorRecebido;
    property SeuNumero: String read FSeuNumero write FSeuNumero;
    property CodTipoOcorrencia: String read FCodTipoOcorrencia write FCodTipoOcorrencia;
    property DescricaoTipoOcorrencia: String read FDescricaoTipoOcorrencia write FDescricaoTipoOcorrencia;
    property MotivoRejeicao: TRetornoRejeicoesTitulo read FMotivoRejeicao write FMotivoRejeicao;

  end;

  { TRetornoBoleto }

  TRetornoBoleto = class(TLibBoletoServiceResposta)
  private
    FCedente: TRetornoDadosCedente;
    FBanco: TRetornoDadosBanco;
    FConta: TRetornoDadosConta;
    FTitulo: TObjectList;

  public
    constructor Create(const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao); reintroduce;
    destructor Destroy; override;

    procedure Processar(const ACBrBoleto: TACBrBoleto); override;

  published
    property Cedente: TRetornoDadosCedente read FCedente write FCedente;
    property Banco: TRetornoDadosBanco read FBanco write FBanco;
    property Conta: TRetornoDadosConta read FConta write FConta;
    property Titulo: TObjectList read FTitulo;

  end;

implementation

uses
  TypInfo, pcnAuxiliar, pcnConversao,
  ACBrUtil, ACBrLibBoletoConsts;

{ TRetornoRejeicoesTitulo }

constructor TRetornoRejeicoesTitulo.Create(const AIDRej: Integer; const AID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
var
  AChave: String;
begin
  AChave := CSessaoMotivoRejeicao + Trim(IntToStrZero(AID + 1, 1)) + Trim(IntToStrZero(AIDRej + 1, 1));

  inherited Create( AChave, ATipo, AFormato);
  FID:= AID;
  FIDRej:= AIDRej;
end;

procedure TRetornoRejeicoesTitulo.Processar(const ACBrBoleto: TACBrBoleto);
begin
  MotivoRejeicao := ACBrBoleto.ListadeBoletos[FID].DescricaoMotivoRejeicaoComando[FIDRej];
end;

{ TRetornoBoleto }

constructor TRetornoBoleto.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoRetorno, ATipo, AFormato);

  FCedente := nil;
  FBanco := nil;
  FConta := nil;
  FTitulo := TObjectList.Create;
end;

destructor TRetornoBoleto.Destroy;
begin
  if Assigned(FCedente) then FreeAndNil(FCedente);
  if Assigned(FBanco) then FreeAndNil(FBanco);
  if Assigned(FConta) then FreeAndNil(FConta);

  FTitulo.Clear;
  FTitulo.Free;

  inherited Destroy;
end;

procedure TRetornoBoleto.Processar(const ACBrBoleto: TACBrBoleto);
var
  I: Integer;
  Titulos: TRetornoDadosTitulo;
begin
  Cedente := TRetornoDadosCedente.Create(Tipo, Formato);
  Cedente.Processar(ACBrBoleto);

  Banco := TRetornoDadosBanco.Create(Tipo, Formato);
  Banco.Processar(ACBrBoleto);

  Conta := TRetornoDadosConta.Create(Tipo, Formato);
  Conta.Processar(ACBrBoleto);

  FTitulo.Clear;
  for I:= 0 to  ACBrBoleto.ListadeBoletos.Count-1 do
  begin
    Titulos := TRetornoDadosTitulo.Create(I, Tipo, Formato);
    Titulos.Processar(ACBrBoleto);
    FTitulo.Add(Titulos);
  end;

end;

{ TRetornoDadosTitulo }

constructor TRetornoDadosTitulo.Create(const AID: Integer; const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoTitulo + Trim(IntToStrZero(AID + 1, 1)), ATipo, AFormato);
  FID := AID;
  FMotivoRejeicao := Nil;

end;

destructor TRetornoDadosTitulo.Destroy;
begin
  if Assigned(FMotivoRejeicao) then FreeAndNil(FMotivoRejeicao);

  inherited Destroy;
end;

procedure TRetornoDadosTitulo.Processar(const ACBrBoleto: TACBrBoleto);
var
  I: Integer;
begin
  if ACBrBoleto.ListadeBoletos.Count > 0 then
  begin
    Sacado_Nome := ACBrBoleto.ListadeBoletos[FID].Sacado.NomeSacado;
    Sacado_CNPJCPF := ACBrBoleto.ListadeBoletos[FID].Sacado.CNPJCPF;
    Vencimento := ACBrBoleto.ListadeBoletos[FID].Vencimento;
    DataDocumento := ACBrBoleto.ListadeBoletos[FID].DataDocumento;
    NumeroDocumento := ACBrBoleto.ListadeBoletos[FID].NumeroDocumento;
    DataProcessamento := ACBrBoleto.ListadeBoletos[FID].DataProcessamento;
    NossoNumero := ACBrBoleto.ListadeBoletos[FID].NossoNumero;
    Carteira := ACBrBoleto.ListadeBoletos[FID].Carteira;
    ValorDocumento := ACBrBoleto.ListadeBoletos[FID].ValorDocumento;
    DataOcorrencia := ACBrBoleto.ListadeBoletos[FID].DataOcorrencia;
    DataCredito := ACBrBoleto.ListadeBoletos[FID].DataCredito;
    DataBaixa := ACBrBoleto.ListadeBoletos[FID].DataBaixa;
    DataMoraJuros := ACBrBoleto.ListadeBoletos[FID].DataMoraJuros;
    ValorDespesaCobranca := ACBrBoleto.ListadeBoletos[FID].ValorDespesaCobranca;
    ValorAbatimento := ACBrBoleto.ListadeBoletos[FID].ValorAbatimento;
    ValorDesconto := ACBrBoleto.ListadeBoletos[FID].ValorDesconto;
    ValorMoraJuros := ACBrBoleto.ListadeBoletos[FID].ValorMoraJuros;
    ValorIOF := ACBrBoleto.ListadeBoletos[FID].ValorIOF;
    ValorOutrasDespesas := ACBrBoleto.ListadeBoletos[FID].ValorOutrasDespesas;
    ValorOutrosCreditos := ACBrBoleto.ListadeBoletos[FID].ValorOutrosCreditos;
    ValorRecebido := ACBrBoleto.ListadeBoletos[FID].ValorRecebido;
    SeuNumero := ACBrBoleto.ListadeBoletos[FID].SeuNumero;
    CodTipoOcorrencia := GetEnumName( TypeInfo(TACBrTipoOcorrencia),
                                             Integer(ACBrBoleto.ListadeBoletos[FID].OcorrenciaOriginal.Tipo));
    DescricaoTipoOcorrencia := ACBrBoleto.ListadeBoletos[FID].OcorrenciaOriginal.Descricao;

    for I:= 0 to  ACBrBoleto.ListadeBoletos[FID].DescricaoMotivoRejeicaoComando.Count-1 do
    begin
      MotivoRejeicao := TRetornoRejeicoesTitulo.Create( I, FID , Tipo, Formato);
      MotivoRejeicao.Processar(ACBrBoleto);
    end;

  end;
end;

{ TRetornoDadosConta }

constructor TRetornoDadosConta.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoConta, ATipo, AFormato);
end;

destructor TRetornoDadosConta.Destroy;
begin
  inherited Destroy;
end;

procedure TRetornoDadosConta.Processar(const ACBrBoleto: TACBrBoleto);
begin
  if Assigned(ACBrBoleto) then
  begin
    Conta := ACBrBoleto.Cedente.Conta;
    DigitoConta := ACBrBoleto.Cedente.ContaDigito;
    Agencia := ACBrBoleto.Cedente.Agencia;
    DigitoAgencia := ACBrBoleto.Cedente.AgenciaDigito;
    DigitoVerificadorAgenciaConta := ACBrBoleto.Cedente.DigitoVerificadorAgenciaConta;

  end;

end;

{ TRetornoDadosBanco }

constructor TRetornoDadosBanco.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoBanco, ATipo, AFormato);
end;

destructor TRetornoDadosBanco.Destroy;
begin
  inherited Destroy;
end;

procedure TRetornoDadosBanco.Processar(const ACBrBoleto: TACBrBoleto);
begin
  if Assigned(ACBrBoleto) then
  begin
    Numero := ACBrBoleto.Banco.Numero;
    IndiceACBr := Integer(ACBrBoleto.Banco.TipoCobranca);
    NumeroCorrespondente := ACBrBoleto.Banco.NumeroCorrespondente;
    VersaoArquivo := ACBrBoleto.Banco.LayoutVersaoArquivo;
    VersaoLote := ACBrBoleto.Banco.LayoutVersaoLote;

  end;

end;

{ TLibBoletoServiceResposta }

constructor TLibBoletoServiceResposta.Create(const ASessao: String;
  const ATipo: TACBrLibRespostaTipo; const AFormato: TACBrLibCodificacao);
begin
  inherited Create(ASessao, ATipo, AFormato);
end;

{ TRetornoDadosCedente }

constructor TRetornoDadosCedente.Create(const ATipo: TACBrLibRespostaTipo;
  const AFormato: TACBrLibCodificacao);
begin
  inherited Create(CSessaoCedente, ATipo, AFormato);
end;

destructor TRetornoDadosCedente.Destroy;
begin
  inherited Destroy;
end;

procedure TRetornoDadosCedente.Processar(const ACBrBoleto: TACBrBoleto);
begin
  if Assigned(ACBrBoleto) then
  begin
    Nome := ACBrBoleto.Cedente.Nome;
    CNPJCPF := ACBrBoleto.Cedente.CNPJCPF;
    CodigoCedente := ACBrBoleto.Cedente.CodigoCedente;
    Modalidade := ACBrBoleto.Cedente.Modalidade;
    CodTransmissao := ACBrBoleto.Cedente.CodigoTransmissao;
    Convenio := ACBrBoleto.Cedente.Convenio;

  end;

end;



end.

