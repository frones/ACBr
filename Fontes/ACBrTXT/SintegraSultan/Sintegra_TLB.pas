{===============================================================================
Projeto Sintegra
Biblioteca de Componente para geração do arquivo Sintegra
Site: http://codigolivre.org.br/projects/sintegra/

Direitos Autorais Reservados (c) 2004 Régys Borges da Silveira

Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la sob
os termos da Licença Pública Geral Menor do GNU conforme publicada pela Free
Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) qualquer
versão posterior.

Esta biblioteca é distribuído na expectativa de que seja útil, porém, SEM
NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU
ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor do
GNU para mais detalhes.

Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto com
esta biblioteca; se não, escreva para a Free Software Foundation, Inc., no
endereço 59 TEmple Street, Suite 330, Boston, MA 02111-1307 USA.
================================================================================
Contato:
          Autor...: Régys Borges da Silveira
          Email...: regyssilveira@hotmail.com
================================================================================
Colaboração:
          Anderson Carli   <anderson@f10.com.br>
          Koplin           <alfredo@k-systems.eti.br>
          Marcelo Welter   <marcelo@welter.pro.br>
===============================================================================}

unit Sintegra_TLB;

interface

uses
  SysUtils, Classes, DateUtils, Dialogs;

type
  TCodIdentificaOper = (opeInterestaduais, opeInterestSubTributaria, opeTotal);
  TCodFinalidade = (finNormal, finRetificacaoTotal, finRetificacaoAditiva,
                    finRetificacaoCorretiva, finDesfazimento);
  TModDocumento = (modMaqRegistradora, modPDV, modECF);
  TSituacao = (nfNormal, nfCancelado, nfExtNormal, nfExtCancelado);
  TTipoEmitente = (tpeProprio, tpeTerceiros);
  TTipoPosse = (tpo1, tpo2, tpo3);
  TModalidadeFrete = (mdfCIF, mdfFOB, mdfOUTROS);
  TArquivoMagnetico = (tamConv1, tamConv2, tamConv3);

  TErrorEvent = procedure(const aErro: string) of object;

  ESintegraException = class(Exception);

  IRegistro1X = interface(IInterface)
    ['{E085C101-C691-4E6F-A85C-DA09FB5E8405}']
    function GetRazaoSocial: string;
    procedure SetRazaoSocial(const Valor: string);
    function GetCNPJ: string;
    procedure SetCNPJ(const Valor: string);
    function GetInscEstadual: string;
    procedure SetInscEstadual(const Valor: string);
    function GetMunicipio: string;
    procedure SetMunicipio(const Valor: string);
    function GetUF: string;
    procedure SetUF(const Valor: string);
    function GetFax: string;
    procedure SetFax(const Valor: string);
    function GetResponsavel: string;
    procedure SetResponsavel(const Valor: string);
    function GetEndereco: string;
    procedure SetEndereco(const Valor: string);
    function GetComplemento: string;
    procedure SetComplemento(const Valor: string);
    function GetNumero: Integer;
    procedure SetNumero(const Valor: Integer);
    function GetBairro: string;
    procedure SetBairro(const Valor: string);
    function GetCEP: string;
    procedure SetCEP(const Valor: string);
    function GetFone: string;
    procedure SetFone(const Valor: string);
    function GetContribuinteIPI: Boolean;
    procedure SetContribuinteIPI(const Valor: Boolean);
    function GetSubstitutoTributario: Boolean;
    procedure SetSubstitutoTributario(const Valor: Boolean);
    property RazaoSocial: string read GetRazaoSocial write SetRazaoSocial;
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property Endereco: string read GetEndereco write SetEndereco;
    property Complemento: string read GetComplemento write SetComplemento;
    property Numero: Integer read GetNumero write SetNumero;
    property Bairro: string read GetBairro write SetBairro;
    property Municipio: string read GetMunicipio write SetMunicipio;
    property CEP: string read GetCEP write SetCEP;
    property UF: string read GetUF write SetUF;
    property Fax: string read GetFax write SetFax;
    property Fone: string read GetFone write SetFone;
    property Responsavel: string read GetResponsavel write SetResponsavel;
    property ContribuinteIPI: Boolean read GetContribuinteIPI write SetContribuinteIPI;
    property SubstitutoTributario: Boolean read GetSubstitutoTributario write SetSubstitutoTributario;
  end;

  IRegistro50 = interface(IInterface)
    ['{F8C8D13E-9F30-4316-A2CE-BB738CF863B1}']
    function GetCFOP: SmallInt;
    procedure SetCFOP(const Valor: SmallInt);
    function GetAliquota: Currency;
    procedure SetAliquota(const Valor: Currency);
    function GetValorTotal: Currency;
    procedure SetValorTotal(const Valor: Currency);
    function GetBaseICMS: Currency;
    procedure SetBaseICMS(const Valor: Currency);
    function GetValorICMS: Currency;
    procedure SetValorICMS(const Valor: Currency);
    function GetIsentasNTribut: Currency;
    procedure SetIsentasNTribut(const Valor: Currency);
    function GetOutras: Currency;
    procedure SetOutras(const Valor: Currency);

    property Aliquota: Currency read GetAliquota write SetAliquota;
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property ValorICMS: Currency read GetValorICMS write SetValorICMS;
    property IsentasNTribut: Currency read GetIsentasNTribut write SetIsentasNTribut;
    property Outras: Currency read GetOutras write SetOutras;
  end;

  IRegistro50Lista = interface(IInterfaceList)
    ['{719351AC-591D-44DC-9A74-FA15A6041F68}']
    function GetItem(Index: Integer): IRegistro50;
    procedure SetItem(Index: Integer; const Value: IRegistro50);
    function New: IRegistro50;

    property Items[Index: Integer]: IRegistro50 read GetItem write SetItem; default;
  end;

  IRegistro51 = interface(IInterface)
    ['{99C19606-A345-49E7-8935-B0A6203305ED}']
    function GetCFOP: SmallInt;
    function GetIsentaNTrib: Currency;
    function GetOutras: Currency;
    function GetValor: Currency;
    function GetValorIPI: Currency;
    procedure SetCFOP(const Value: SmallInt);
    procedure SetIsentaNTrib(const Value: Currency);
    procedure SetOutras(const Value: Currency);
    procedure SetValor(const Value: Currency);
    procedure SetValorIPI(const Value: Currency);
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property Valor: Currency read GetValor write SetValor;
    property ValorIPI: Currency read GetValorIPI write SetValorIPI;
    property IsentaNTrib: Currency read GetIsentaNTrib write SetIsentaNTrib;
    property Outras: Currency read GetOutras write SetOutras;
  end;

  IRegistro51Lista = interface(IInterfaceList)
    ['{94A15C05-7DE0-45F8-9EFF-BC86B04E19DE}']
    function GetItem(Index: Integer): IRegistro51;
    procedure SetItem(Index: Integer; const Value: IRegistro51);
    function New: IRegistro51;
    property Items[Index: Integer]: IRegistro51 read GetItem write SetItem; default;
  end;

  IRegistro53 = interface(IInterface)
    ['{012F65C2-EE5A-4335-B53E-285722A3D346}']
    function GetBaseCalcICMSST: Currency;
    function GetCFOP: SmallInt;
    function GetDespAcessoria: Currency;
    function GetICMSRetido: Currency;
    function GetCodAntecipacao: string;
    procedure SetBaseCalcICMSST(const Valor: Currency);
    procedure SetCFOP(const Valor: SmallInt);
    procedure SetDespAcessoria(const Valor: Currency);
    procedure SetICMSRetido(const Valor: Currency);
    procedure SetCodAntecipacao(const Value: string);
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property BaseCalcICMSST: Currency read GetBaseCalcICMSST write SetBaseCalcICMSST;
    property ICMSRetido: Currency read GetICMSRetido write SetICMSRetido;
    property DespAcessoria: Currency read GetDespAcessoria write SetDespAcessoria;
    property CodAntecipacao: string read GetCodAntecipacao write SetCodAntecipacao;
  end;

  IRegistro53Lista = interface(IInterfaceList)
    ['{6A4380F2-EC19-4C44-9A03-FD03862764F9}']
    function GetItem(Index: Integer): IRegistro53;
    procedure SetItem(Index: Integer; const Value: IRegistro53);
    function New: IRegistro53;
    property Items[Index: Integer]: IRegistro53 read GetItem write SetItem; default;
  end;

  IRegistro54 = interface(IInterface)
    ['{81F347A8-5398-46E0-8125-9FE5E3044538}']
    function GetAliquota: Currency;
    function GetBaseICMS: Currency;
    function GetBaseICMSST: Currency;
    function GetCodProduto: string;
    function GetCST: string;
    function GetDesconto: Currency;
    function GetIPI: Currency;
    function GetNumItem: SmallInt;
    function GetQuantidade: Extended;
    function GetValorProduto: Currency;
    procedure SetAliquota(const Valor: Currency);
    procedure SetBaseICMS(const Valor: Currency);
    procedure SetBaseICMSST(const Valor: Currency);
    procedure SetCodProduto(const Valor: string);
    procedure SetCST(const Valor: string);
    procedure SetDesconto(const Valor: Currency);
    procedure SetIPI(const Valor: Currency);
    procedure SetNumItem(const Valor: SmallInt);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetValorProduto(const Valor: Currency);
    function GetCFOP: SmallInt;
    procedure SetCFOP(const Valor: SmallInt);
    property NumItem: SmallInt read GetNumItem write SetNumItem;
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property CST: string read GetCST write SetCST;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorProduto: Currency read GetValorProduto write SetValorProduto;
    property Desconto: Currency read GetDesconto write SetDesconto;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property BaseICMSST: Currency read GetBaseICMSST write SetBaseICMSST;
    property IPI: Currency read GetIPI write SetIPI;
    property Aliquota: Currency read GetAliquota write SetAliquota;
  end;

  IRegistro54Lista = interface(IInterfaceList)
    ['{0D6C76B8-7979-460B-A247-0E31523A8954}']
    function GetItem(Index: Integer): IRegistro54;
    procedure SetItem(Index: Integer; const Value: IRegistro54);
    function New: IRegistro54;
    property Items[Index: Integer]: IRegistro54 read GetItem write SetItem; default;
  end;

  IRegistro55 = interface(IInterface)
    ['{4C944F8E-62A9-4DD6-AA39-BA7E6FBBA1A7}']
    function GetAgencia: SmallInt;
    function GetAnoRef: SmallInt;
    function GetBanco: SmallInt;
    function GetDataPagamento: TDateTime;
    function GetMesRef: SmallInt;
    function GetNumConvenio: string;
    function GetNumeroGNRE: string;
    function GetUFContribuinte: string;
    function GetUFFavorecido: string;
    function GetValor: Currency;
    function GetVencimento: TDateTime;
    procedure SetAgencia(const Valor: SmallInt);
    procedure SetAnoRef(const Valor: SmallInt);
    procedure SetBanco(const Valor: SmallInt);
    procedure SetDataPagamento(const Valor: TDateTime);
    procedure SetMesRef(const Valor: SmallInt);
    procedure SetNumConvenio(const Valor: string);
    procedure SetNumeroGNRE(const Valor: string);
    procedure SetUFContribuinte(const Valor: string);
    procedure SetUFFavorecido(const Valor: string);
    procedure SetValor(const Valor: Currency);
    procedure SetVencimento(const Valor: TDateTime);
    property MesRef: SmallInt read GetMesRef write SetMesRef;
    property AnoRef: SmallInt read GetAnoRef write SetAnoRef;
    property NumConvenio: string read GetNumConvenio write SetNumConvenio;
    property Vencimento: TDateTime read GetVencimento write SetVencimento;
    property DataPagamento: TDateTime read GetDataPagamento write SetDataPagamento;
    property UFContribuinte: string read GetUFContribuinte write SetUFContribuinte;
    property UFFavorecido: string read GetUFFavorecido write SetUFFavorecido;
    property Banco: SmallInt read GetBanco write SetBanco;
    property Agencia: SmallInt read GetAgencia write SetAgencia;
    property NumeroGNRE: string read GetNumeroGNRE write SetNumeroGNRE;
    property Valor: Currency read GetValor write SetValor;
  end;

  IRegistro55Lista = interface(IInterfaceList)
    ['{8DABFC2E-08BA-492C-9CD5-13C51C3D3F91}']
    function GetItem(Index: Integer): IRegistro55;
    procedure SetItem(Index: Integer; const Value: IRegistro55);
    function New: IRegistro55;
    property Items[Index: Integer]: IRegistro55 read GetItem write SetItem; default;
  end;

  IRegistro71 = interface(IInterface)
    ['{469F4E7D-DEA8-40A1-BE27-F08C99B2ECE1}']
    function GetCNPJ: string;
    procedure SetCNPJ(const Valor: string);
    function GetInscEstadual: string;
    procedure SetInscEstadual(const Valor: string);
    function GetData: TDateTime;
    procedure SetData(const Valor: TDateTime);
    function GetUF: string;
    procedure SetUF(const Valor: string);
    function GetModelo: SmallInt;
    procedure SetModelo(const Valor: SmallInt);
    function GetSerie: string;
    procedure SetSerie(const Valor: string);
    function GetNumero: Integer;
    procedure SetNumero(const Valor: Integer);
    function GetValorTotal: Currency;
    procedure SetValorTotal(const Value: Currency);
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property Data: TDateTime read GetData write SetData;
    property UF: string read GetUF write SetUF;
    property Modelo: SmallInt read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
    property Numero: Integer read GetNumero write SetNumero;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
  end;

  IRegistro71Lista = interface(IInterfaceList)
    ['{4AF3F1E1-F645-45EF-8A23-5F0437E93C58}']
    function GetItem(Index: Integer): IRegistro71;
    procedure SetItem(Index: Integer; const Value: IRegistro71);
    function New: IRegistro71;
    property Items[Index: Integer]: IRegistro71 read GetItem write SetItem; default;
  end;

  IRegistro5X = interface(IInterface)
    ['{9E5AD819-C09B-4BC7-A917-B6C175215FCF}']
    function GetCFOP: SmallInt;
    procedure SetCFOP(const Valor: SmallInt);
    function GetCNPJ: string;
    procedure SetCNPJ(const Valor: string);
    function GetInscEstadual: string;
    procedure SetInscEstadual(const Valor: string);
    function GetData: TDateTime;
    procedure SetData(const Valor: TDateTime);
    function GetUF: string;
    procedure SetUF(const Valor: string);
    function GetModelo: SmallInt;
    procedure SetModelo(const Valor: SmallInt);
    function GetSerie: string;
    procedure SetSerie(const Valor: string);
    function GetNumero: Integer;
    procedure SetNumero(const Valor: Integer);
    function GetTipoEmitente: TTipoEmitente;
    procedure SetTipoEmitente(const Valor: TTipoEmitente);
    function GetSituacao: TSituacao;
    procedure SetSituacao(const Valor: TSituacao);
    function GetRegistro54: IRegistro54Lista;
    procedure SetRegistro54(const Valor: IRegistro54Lista);
    function GetRegistro50: IRegistro50Lista;
    procedure SetRegistro50(const Valor: IRegistro50Lista);
    function GetDespAcessoria: Currency;
    procedure SetDespAcessoria(const Valor: Currency);
    function GetValorPisCofins: Currency;
    procedure SetValorPisCofins(const Valor: Currency);
    function GetValorComplementar: Currency;
    procedure SetValorComplementar(const Valor: Currency);
    function GetValorServicoNaoTributado: Currency;
    procedure SetValorServicoNaoTributado(const Valor: Currency);
    function GetFrete: Currency;
    procedure SetFrete(const Valor: Currency);
    function GetSeguro: Currency;
    procedure SetSeguro(const Valor: Currency);
    function GetRegistro51: IRegistro51Lista;
    procedure SetRegistro51(const Valor: IRegistro51Lista);
    function GetRegistro53: IRegistro53Lista;
    procedure SetRegistro53(const Valor: IRegistro53Lista);
    function GetModFrete: TModalidadeFrete;
    function GetSubSerie: string;
    procedure SetModFrete(const Value: TModalidadeFrete);
    procedure SetSubSerie(const Value: string);
    function GetRegistro71: IRegistro71Lista;
    procedure SetRegistro71(const Valor: IRegistro71Lista);
    property CFOP: SmallInt read GetCFOP write SetCFOP;
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property Data: TDateTime read GetData write SetData;
    property UF: string read GetUF write SetUF;
    property Modelo: SmallInt read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
    property SubSerie: string read GetSubSerie write SetSubSerie;
    property Numero: Integer read GetNumero write SetNumero;
    property TipoEmitente: TTipoEmitente read GetTipoEmitente write SetTipoEmitente;
    property ModFrete: TModalidadeFrete read GetModFrete write SetModFrete;
    property Frete: Currency read GetFrete write SetFrete;
    property Seguro: Currency read GetSeguro write SetSeguro;
    property DespAcessoria: Currency read GetDespAcessoria write SetDespAcessoria;
    property ValorPisCofins: Currency read GetValorPisCofins write SetValorPisCofins;
    property ValorComplementar: Currency read GetValorComplementar write SetValorComplementar;
    property ValorServicoNaoTributado: Currency read GetValorServicoNaoTributado write SetValorServicoNaoTributado;
    property Situacao: TSituacao read GetSituacao write SetSituacao;
    property Registro50: IRegistro50Lista read GetRegistro50 write SetRegistro50;
    property Registro54: IRegistro54Lista read GetRegistro54 write SetRegistro54;
    property Registro51: IRegistro51Lista read GetRegistro51 write SetRegistro51;
    property Registro53: IRegistro53Lista read GetRegistro53 write SetRegistro53;
    property Registro71: IRegistro71Lista read GetRegistro71 write SetRegistro71;
  end;

  IRegistro5XLista = interface(IInterfaceList)
    ['{93242FA7-79B6-4584-8AE2-DE6585D680A9}']
    function GetItem(Index: Integer): IRegistro5X;
    procedure SetItem(Index: Integer; const Value: IRegistro5X);
    function Gettotalentradas: Integer;
    function GetTotalSaidas: Integer;
    function New: IRegistro5X;
    property Items[Index: Integer]: IRegistro5X read GetItem write SetItem; default;
    property TotalEntradas: Integer read Gettotalentradas;
    property TotalSaidas: Integer read GetTotalSaidas;
  end;

  IRegistro60A = interface(IInterface)
    ['{EDEFBBD5-CFD3-45E4-BB64-9FCB0A500330}']
    function GetSitTributaria: string;
    procedure SetSitTributaria(const Valor: string);
    function GetValorAcumulado: Currency;
    procedure SetValorAcumulado(const Valor: Currency);
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
    property ValorAcumulado: Currency read GetValorAcumulado write SetValorAcumulado;
  end;

  IRegistro60ALista = interface(IInterfaceList)
    ['{C6B68871-908B-4CD7-BF4C-1AA16BA2CD71}']
    function GetItem(Index: Integer): IRegistro60A;
    procedure SetItem(Index: Integer; const Value: IRegistro60A);
    function Total: Currency;
    function New: IRegistro60A;
    property Items[Index: Integer]: IRegistro60A read GetItem write SetItem; default;
  end;

  IRegistro60I = interface(IInterface)
    ['{3959B461-1EE9-4758-BC07-AFA2D8CFAF60}']
    function GetBaseICMS: Currency;
    function GetCodProduto: string;
    function GetCOOCupom: Integer;
    function GetNumItem: Integer;
    function GetQuantidade: Extended;
    function GetSitTributaria: string;
    function GetValorICMS: Currency;
    function GetValorUnitario: Currency;
    procedure SetCodProduto(const Valor: string);
    procedure SetCOOCupom(const Valor: Integer);
    procedure SetNumItem(const Valor: Integer);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetSitTributaria(const Valor: string);
    procedure SetValorICMS(const Valor: Currency);
    procedure SetValorUnitario(const Valor: Currency);
    procedure SetBaseICMS(const Valor: Currency);
    property COOCupom: Integer read GetCOOCupom write SetCOOCupom;
    property NumItem: Integer read GetNumItem write SetNumItem;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorUnitario: Currency read GetValorUnitario write SetValorUnitario;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
    property ValorICMS: Currency read GetValorICMS write SetValorICMS;
  end;

  IRegistro60ILista = interface(IInterfaceList)
    ['{32BED6DE-668F-4EE9-984F-5F7DD2D91B56}']
    function GetItem(Index: Integer): IRegistro60I;
    procedure SetItem(Index: Integer; const Value: IRegistro60I);
    function New: IRegistro60I;
    property Items[Index: Integer]: IRegistro60I read GetItem write SetItem; default;
  end;

  IRegistro60D = interface(IInterface)
    ['{CDC656A0-5B10-4D40-B806-277473AB2F19}']
    function GetSitTributaria: string;
    function GetBaseCalcICMS: Currency;
    function GetCodProduto: string;
    function GetQuantAcumulada: Extended;
    function GetValorAcumulado: Currency;
    function GetValorICMS: Currency;
    procedure SetSitTributaria(const Valor: string);
    procedure SetBaseCalcICMS(const Valor: Currency);
    procedure setCodProduto(const Valor: string);
    procedure SetQuantAcumulada(const Valor: Extended);
    procedure SetValorAcumulado(const Valor: Currency);
    property CodProduto: string read GetCodProduto write setCodProduto;
    property QuantAcumulada: Extended read GetQuantAcumulada write SetQuantAcumulada;
    property ValorAcumulado: Currency read GetValorAcumulado write SetValorAcumulado;
    property BaseCalcICMS: Currency read GetBaseCalcICMS write SetBaseCalcICMS;
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
    property ValorICMS: Currency read GetValorICMS;
  end;

  IRegistro60DLista = interface(IInterfaceList)
    ['{82479ECB-009C-4DCA-A8EA-46168A9B42A4}']
    function GetItem(Index: Integer): IRegistro60D;
    procedure SetItem(Index: Integer; const Value: IRegistro60D);
    function New: IRegistro60D;
    function GetTotalAcumulado: Currency;
    property Items[Index: Integer]: IRegistro60D read GetItem write SetItem; default;
    property TotalAcumulado: Currency read GetTotalAcumulado;
  end;

  IRegistro60R = interface(IInterface)
    ['{A846B50C-E2A0-4B4A-887C-E30CD6DE0B46}']
    function GetAno: SmallInt;
    function GetCodProduto: string;
    function GetMes: SmallInt;
    function GetQuantidade: Extended;
    function GetSitTributaria: string;
    function GetValorAcumICMS: Currency;
    function GetValorAcumProduto: Currency;
    procedure SetAno(const Valor: SmallInt);
    procedure SetCodProduto(const Valor: string);
    procedure SetMes(const Valor: SmallInt);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetSitTributaria(const Valor: string);
    procedure SetValorAcumICMS(const Valor: Currency);
    procedure SetValorAcumProduto(const Valor: Currency);
    property Mes: SmallInt read GetMes write SetMes;
    property ANo: SmallInt read GetAno write SetAno;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorAcumProduto: Currency read GetValorAcumProduto write SetValorAcumProduto;
    property ValorAcumICMS: Currency read GetValorAcumICMS write SetValorAcumICMS;
    property SitTributaria: string read GetSitTributaria write SetSitTributaria;
  end;

  IRegistro60RLista = interface(IInterfaceList)
    ['{4AF3F1E1-F645-45EF-8A23-5F0437E93C58}']
    function GetItem(Index: Integer): IRegistro60R;
    procedure SetItem(Index: Integer; const Value: IRegistro60R);
    function New: IRegistro60R;
    property Items[Index: Integer]: IRegistro60R read GetItem write SetItem; default;
  end;

  IRegistro60M = interface(IInterface)
    ['{1EA59280-823F-4149-81DA-5992AD482562}']
    function GetDataEmissao: TDateTime;
    procedure SetDataEmissao(const Valor: TDateTime);
    function GetNumSerieEquip: string;
    procedure SetNumSerieEquip(const Valor: string);
    function GetNumSequencial: Integer;
    procedure SetNumSequencial(const Valor: Integer);
    function GetModDocFiscal: TModDocumento;
    procedure SetModDocFiscal(const Valor: TModDocumento);
    function GetCOOInicial: Integer;
    procedure SetCOOInicial(const Valor: Integer);
    function GetCOOFinal: Integer;
    procedure SetCOOFinal(const Valor: Integer);
    function GetContReducaoZ: Integer;
    procedure SetContReducaoZ(const Valor: Integer);
    function GetContReinicioOper: Integer;
    procedure SetContReinicioOper(const Valor: Integer);
    function GetVendaBruta: Currency;
    procedure SetVendaBruta(const Valor: Currency);
    function GetGTFinal: Currency;
    procedure SetGTFinal(const Valor: Currency);
    function GetRegistro60A: IRegistro60ALista;
    procedure SetRegistro60A(const Valor: IRegistro60ALista);
    function GetRegistro60D: IRegistro60DLista;
    procedure SetRegistro60D(const Valor: IRegistro60DLista);
    function GetRegistro60I: IRegistro60ILista;
    procedure SetRegistro60I(const Valor: IRegistro60ILista);
    property DataEmissao: TDateTime read GetDataEmissao write SetDataEmissao;
    property NumSerieEquip: string read GetNumSerieEquip write SetNumSerieEquip;
    property NumSequencial: Integer read GetNumSequencial write SetNumSequencial;
    property ModDocFiscal: TModDocumento read GetModDocFiscal write SetModDocFiscal;
    property COOInicial: Integer read GetCOOInicial write SetCOOInicial;
    property COOFinal: Integer read GetCOOFinal write SetCOOFinal;
    property ContReducaoZ: Integer read GetContReducaoZ write SetContReducaoZ;
    property ContReinicioOper: Integer read GetContReinicioOper write SetContReinicioOper;
    property VendaBruta: Currency read GetVendaBruta write SetVendaBruta;
    property GTFinal: Currency read GetGTFinal write SetGTFinal;
    property Registro60A: IRegistro60ALista read GetRegistro60A write SetRegistro60A;
    property Registro60D: IRegistro60DLista read GetRegistro60D write SetRegistro60D;
    property Registro60I: IRegistro60ILista read GetRegistro60I write SetRegistro60I;
  end;

  IRegistro60MList = interface(IInterfaceList)
    ['{5497071C-DC2B-4E69-90FB-354C041EB46F}']
    function GetItem(Index: Integer): IRegistro60M;
    procedure SetItem(Index: Integer; const Value: IRegistro60M);
    function New: IRegistro60M;
    property Items[Index: Integer]: IRegistro60M read GetItem write SetItem; default;
  end;

  IRegistro61 = interface(IInterface)
    ['{55AA430D-AC39-4FFC-B9EB-6F0E5E5989D2}']
    function GetDataEmissao: TDateTime;
    procedure SetDataEmissao(const Valor: TDateTime);
    function GetModeloDoc: Integer;
    procedure SetModeloDoc(const Valor: Integer);
    function GetSerie: string;
    procedure SetSerie(const Valor: string);
    function GetSubSerie: string;
    procedure SetSubSerie(const Valor: string);
    function GetNumInicial: Integer;
    procedure SetNumInicial(const Valor: Integer);
    function GetNumFinal: Integer;
    procedure SetNumFinal(const Valor: Integer);
    function GetValorTotal: Currency;
    procedure SetValorTotal(const Valor: Currency);
    function GetBaseICMS: Currency;
    procedure SetBaseICMS(const Valor: Currency);
    function GetIsentasNTrib: Currency;
    procedure SetIsentasNTrib(const Valor: Currency);
    function GetOutras: Currency;
    procedure SetOutras(const Valor: Currency);
    function GetAliquota: Currency;
    procedure SetAliquota(const Valor: Currency);
    function GetValorICMS: Currency;
    property DataEmissao: TDateTime read GetDataEmissao write SetDataEmissao;
    property ModeloDoc: Integer read GetModeloDoc write SetModeloDoc;
    property Serie: string read GetSerie write SetSerie;
    property SubSerie: string read GetSubSerie write SetSubSerie;
    property NumInicial: Integer read GetNumInicial write SetNumInicial;
    property NumFinal: Integer read GetNumFinal write SetNumFinal;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
    property BaseICMS: Currency read GetBaseICMS write SetBaseICMS;
    property IsentasNTrib: Currency read GetIsentasNTrib write SetIsentasNTrib;
    property Outras: Currency read GetOutras write SetOutras;
    property Aliquota: Currency read GetAliquota write SetAliquota;
    property ValorICMS: currency read GetValorICMS;
  end;

  IRegistro61Lista = interface(IInterfaceList)
    ['{C235B185-A1EB-4AFB-AEE9-15AE857E1A85}']
    function GetItem(Index: Integer): IRegistro61;
    procedure SetItem(Index: Integer; const Value: IRegistro61);
    function New: IRegistro61;
    property Items[Index: Integer]: IRegistro61 read GetItem write SetItem; default;
  end;

  IRegistro61R = interface(IInterface)
    ['{DE7A9950-E87D-4755-9569-F5420E811649}']
    function GetMes: SmallInt;
    function GetAno: SmallInt;
    function GetCodProduto: string;
    function GetQuantidade: Extended;
    function GetValorBrutoProduto: Currency;
    function GetBaseCalcICMS: Currency;
    function GetAliquota: Currency;
    procedure SetMes(const Valor: SmallInt);
    procedure SetAno(const Valor: SmallInt);
    procedure SetCodProduto(const Valor: string);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetValorBrutoProduto(const Valor: Currency);
    procedure SetBaseCalcICMS(const Valor: Currency);
    procedure SetAliquota(const Valor: Currency);
    property Mes: SmallInt read GetMes write SetMes;
    property Ano: SmallInt read GetAno write SetAno;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorBrutoProduto: Currency read GetValorBrutoProduto write SetValorBrutoProduto;
    property BaseCalcICMS: Currency read GetBaseCalcICMS write SetBaseCalcICMS;
    property Aliquota: Currency read GetAliquota write SetAliquota;
  end;

  IRegistro61RLista = interface(IInterfaceList)
    ['{9ED9097C-B06C-492E-BA79-D1618B5A2077}']
    function GetItem(Index: Integer): IRegistro61R;
    procedure SetItem(Index: Integer; const Value: IRegistro61R);
    function New: IRegistro61R;
    property Items[Index: Integer]: IRegistro61R read GetItem write SetItem; default;
  end;

  IRegistro74 = interface(IInterface)
    ['{A9FD144C-BE79-4E13-A1E9-CB99D2626E4D}']
    function GetCNPJ: string;
    function GetCodPRoduto: string;
    function GetDataInventario: TDateTime;
    function GetInscEstadual: string;
    function GetQuantidade: Extended;
    function GetTipoPosse: TTipoPosse;
    function GetUF: string;
    function GetValorTotal: Currency;
    procedure SetCNPJ(const Valor: string);
    procedure SetCodPRoduto(const Valor: string);
    procedure SetDataInventario(const Valor: TDateTime);
    procedure SetInscEstadual(const Valor: string);
    procedure SetQuantidade(const Valor: Extended);
    procedure SetTipoPosse(const Valor: TTipoPosse);
    procedure SetUF(const Valor: string);
    procedure SetValorTotal(const Valor: Currency);
    property TipoPosse: TTipoPosse read GetTipoPosse write SetTipoPosse;
    property CNPJ: string read GetCNPJ write SetCNPJ;
    property InscEstadual: string read GetInscEstadual write SetInscEstadual;
    property UF: string read GetUF write SetUF;
    property DataInventario: TDateTime read GetDataInventario write SetDataInventario;
    property CodPRoduto: string read GetCodPRoduto write SetCodPRoduto;
    property Quantidade: Extended read GetQuantidade write SetQuantidade;
    property ValorTotal: Currency read GetValorTotal write SetValorTotal;
  end;

  IRegistro74Lista = interface(IInterfaceList)
    ['{CD2F0EE7-C5BE-41E2-A9FB-E8839E9260CF}']
    function GetItem(Index: Integer): IRegistro74;
    procedure SetItem(Index: Integer; const Value: IRegistro74);
    function New: IRegistro74;
    property Items[Index: Integer]: IRegistro74 read GetItem write SetItem; default;
  end;

  IRegistro75 = interface(IInterface)
    ['{86306321-1EE9-442D-8585-D9B5B9187572}']
    function GetAliquotaICMS: Currency;
    function GetAliquotaIPI: Currency;
    function GetBaseICMSST: Currency;
    function GetCodNCM: string;
    function GetCodProduto: string;
    function GetDescricao: string;
    function GetReducaoBaseCalc: Currency;
    function GetUnidade: string;
    function GetValidadeFinal: TDateTime;
    function GetValidadeInicial: TDateTime;
    procedure SetAliquotaICMS(const Valor: Currency);
    procedure SetAliquotaIPI(const Valor: Currency);
    procedure SetBaseICMSST(const Valor: Currency);
    procedure SetCodNCM(const Valor: string);
    procedure SetCodProduto(const Valor: string);
    procedure SetDescricao(const Valor: string);
    procedure SetReducaoBaseCalc(const Valor: Currency);
    procedure SetUnidade(const Valor: string);
    procedure SetValidadeFinal(const Valor: TDateTime);
    procedure SetValidadeInicial(const Valor: TDateTime);
    property ValidadeInicial: TDateTime read GetValidadeInicial write SetValidadeInicial;
    property ValidadeFinal: TDateTime read GetValidadeFinal write SetValidadeFinal;
    property CodProduto: string read GetCodProduto write SetCodProduto;
    property CodNCM: string read GetCodNCM write SetCodNCM;
    property Descricao: string read GetDescricao write SetDescricao;
    property Unidade: string read GetUnidade write SetUnidade;
    property AliquotaIPI: Currency read GetAliquotaIPI write SetAliquotaIPI;
    property AliquotaICMS: Currency read GetAliquotaICMS write SetAliquotaICMS;
    property ReducaoBaseCalc: Currency read GetReducaoBaseCalc write SetReducaoBaseCalc;
    property BaseICMSST: Currency read GetBaseICMSST write SetBaseICMSST;
  end;

  IRegistro75Lista = interface(IInterfaceList)
    ['{7621A167-AB53-4CF2-8AB3-E538F4648E60}']
    function GetItem(Index: Integer): IRegistro75;
    procedure SetItem(Index: Integer; const Value: IRegistro75);
    function New: IRegistro75;
    function ExisteProduto(const AProduto: IRegistro75): Integer;
    property Items[Index: Integer]: IRegistro75 read GetItem write SetItem; default;
  end;

  IRegistro85 = interface(IInterface)
    ['{66D12132-65D1-41B6-99F8-B9B7E567E16C}']
    function GetConhecimento: string;
    function GetDataAverbacao: TDateTime;
    function GetDataConhecimento: TDateTime;
    function GetDataDeclaracao: TDateTime;
    function GetDataNotaFiscal: TDateTime;
    function GetDataRegistro: TDateTime;
    function GetDeclaracao: String;
    function GetModelo: string;
    function GetNaturezaExportacao: string;
    function GetNumeroNotaFiscal: string;
    function GetPais: string;
    function GetRegistroExportacao: String;
    function GetSerie: string;
    function GetTipoConhecimento: String;
    procedure SetConhecimento(const Value: string);
    procedure SetDataAverbacao(const Value: TDateTime);
    procedure SetDataConhecimento(const Value: TDateTime);
    procedure SetDataDeclaracao(const Value: TDateTime);
    procedure SetDataNotaFiscal(const Value: TDateTime);
    procedure SetDataRegistro(const Value: TDateTime);
    procedure SetDeclaracao(const Value: String);
    procedure SetModelo(const Value: string);
    procedure SetNaturezaExportacao(const Value: string);
    procedure SetNumeroNotaFiscal(const Value: string);
    procedure SetPais(const Value: string);
    procedure SetRegistroExportacao(const Value: String);
    procedure SetSerie(const Value: string);
    procedure SetTipoConhecimento(const Value: String);
    property Declaracao: String read GetDeclaracao write SetDeclaracao;
    property DataDeclaracao: TDateTime read GetDataDeclaracao write SetDataDeclaracao;
    property NaturezaExportacao: string read GetNaturezaExportacao write SetNaturezaExportacao;
    property RegistroExportacao: String read GetRegistroExportacao write SetRegistroExportacao;
    property DataRegistro: TDateTime read GetDataRegistro write SetDataRegistro;
    property Conhecimento: string read GetConhecimento write SetConhecimento;
    property DataConhecimento: TDateTime read GetDataConhecimento write SetDataConhecimento;
    property TipoConhecimento: String read GetTipoConhecimento write SetTipoConhecimento;
    property Pais: string read GetPais write SetPais;
    property DataAverbacao: TDateTime read GetDataAverbacao write SetDataAverbacao;
    property NumeroNotaFiscal: string read GetNumeroNotaFiscal write SetNumeroNotaFiscal;
    property DataNotaFiscal: TDateTime read GetDataNotaFiscal write SetDataNotaFiscal;
    property Modelo: string read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
  end;

  IRegistro85Lista = interface(IInterfaceList)
    ['{B7EACE6D-4E66-4ADB-BE6E-BEE8A6A05685}']
    function GetItem(Index: Integer): IRegistro85;
    procedure SetItem(Index: Integer; const Value: IRegistro85);
    function New: IRegistro85;
    property Items[Index: Integer]: IRegistro85 read GetItem write SetItem; default;
  end;

  IRegistro86 = interface(IInterface)
    ['{3181B862-AF9A-44E8-AF08-5AC2892BC91A}']
    function GetCodigo: string;
    function GetCPFCNPJ: string;
    function GetDataDocumento: TDateTime;
    function GetDataRegistro: TDateTime;
    function GetInscricao: string;
    function GetModelo: string;
    function GetNumeroNotaFiscal: string;
    function GetQuantidade: Currency;
    function GetRegistroExportacao: string;
    function GetRelacionamento: string;
    function GetSerie: string;
    function GetUF: string;
    function GetValorTotalProduto: Currency;
    function GetValorUnitario: Currency;
    procedure SetCodigo(const Value: string);
    procedure SetCPFCNPJ(const Value: string);
    procedure SetDataDocumento(const Value: TDateTime);
    procedure SetDataRegistro(const Value: TDateTime);
    procedure SetInscricao(const Value: string);
    procedure SetModelo(const Value: string);
    procedure SetNumeroNotaFiscal(const Value: string);
    procedure SetQuantidade(const Value: Currency);
    procedure SetRegistroExportacao(const Value: string);
    procedure SetRelacionamento(const Value: string);
    procedure SetSerie(const Value: string);
    procedure SetUF(const Value: string);
    procedure SetValorTotalProduto(const Value: Currency);
    procedure SetValorUnitario(const Value: Currency);
    property RegistroExportacao: string read GetRegistroExportacao write SetRegistroExportacao;
    property DataRegistro: TDateTime read GetDataRegistro write SetDataRegistro;
    property CPFCNPJ: string read GetCPFCNPJ write SetCPFCNPJ;
    property Inscricao: string read GetInscricao write SetInscricao;
    property UF: string read GetUF write SetUF;
    property NumeroNotaFiscal: string read GetNumeroNotaFiscal write SetNumeroNotaFiscal;
    property DataDocumento: TDateTime read GetDataDocumento write SetDataDocumento;
    property Modelo: string read GetModelo write SetModelo;
    property Serie: string read GetSerie write SetSerie;
    property Codigo: string read GetCodigo write SetCodigo;
    property Quantidade: Currency read GetQuantidade write SetQuantidade;
    property ValorUnitario: Currency read GetValorUnitario write SetValorUnitario;
    property ValorTotalProduto: Currency read GetValorTotalProduto write SetValorTotalProduto;
    property Relacionamento: string read GetRelacionamento write SetRelacionamento;
  end;

  IRegistro86Lista = interface(IInterfaceList)
    ['{856DC714-1BAB-48A2-A8E6-0EF8B40DE15A}']
    function GetItem(Index: Integer): IRegistro86;
    procedure SetItem(Index: Integer; const Value: IRegistro86);
    function New: IRegistro86;
    property Items[Index: Integer]: IRegistro86 read GetItem write SetItem; default;
  end;

  ISintegra = interface(IInterface)
    ['{FE7CCEC6-64BB-4CB7-9F50-EEC8B35147D2}']
    function GetRegistro60M: IRegistro60MList;
    function GetRegistro60R: IRegistro60RLista;
    function GetDataFinal: TDateTime;
    function GetDataInicial: TDateTime;
    function GetRegistro1X: IRegistro1X;
    function GetFinalidade: TCodFinalidade;
    function GetRegistro55: IRegistro55Lista;
    function GetRegistro74: IRegistro74Lista;
    function GetNaturezaOperacao: TCodIdentificaOper;
    function GetArquivoMagnetico: TArquivoMagnetico;
    function GetRegistro5X: IRegistro5XLista;
    function GetRegistro61: IRegistro61Lista;
    function GetRegistro61R: IRegistro61RLista;
    function GetRegistro75: IRegistro75Lista;
    function GetVersao: string;
    procedure SetRegistro60M(const Valor: IRegistro60MList);
    procedure SetRegistro60R(const Valor: IRegistro60RLista);
    procedure SetDataFinal(const Valor: TDateTime);
    procedure SetDataInicial(const Valor: TDateTime);
    procedure SetRegistro1X(const Valor: IRegistro1X);
    procedure SetFinalidade(const Valor: TCodFinalidade);
    procedure SetRegistro55(const Valor: IRegistro55Lista);
    procedure SetRegistro74(const Valor: IRegistro74Lista);
    procedure SetNaturezaOperacao(const Valor: TCodIdentificaOper);
    procedure SetArquivoMagnetico(const Value: TArquivoMagnetico);
    procedure SetRegistro5X(const Valor: IRegistro5XLista);
    procedure SetRegistro61(const Valor: IRegistro61Lista);
    procedure SetRegistro61R(const Valor: IRegistro61RLista);
    procedure SetRegistro75(const Valor: IRegistro75Lista);
    function GetOnErro: TErrorEvent;
    procedure SetOnErro(const Value: TErrorEvent);
    procedure GerarErro(MensagemErro: String);
    procedure LimparRegistros;
    function VerificaProduto(aProduto: string): Boolean;
    function GerarArquivo(aArquivo: string): Boolean;
    procedure Check(Condicao: Boolean; const Msg: string); overload;
    procedure Check(Condicao: Boolean; Msg: string; Fmt: array of const); overload;
    procedure Check(const StrNaoNula, Msg: string); overload;
    procedure Check(const StrNaoNula, Msg: string; Fmt: array of const); overload;
    procedure Check(Valor, Minimo, Maximo: Double; const Msg: string); overload;
    procedure Check(Valor, Minimo, Maximo: Double; const Msg: string; Fmt: array of const); overload;
    property OnErro: TErrorEvent read GetOnErro write SetOnErro;
    property Versao: string read GetVersao;
    property DataInicial: TDateTime read GetDataInicial write SetDataInicial;
    property DataFinal: TDateTime read GetDataFinal write SetDataFinal;
    property NatOperacao: TCodIdentificaOper read GetNaturezaOperacao write SetNaturezaOperacao;
    property Finalidade: TCodFinalidade read GetFinalidade write SetFinalidade;
    property ArquivoMagnetico: TArquivoMagnetico read GetArquivoMagnetico write SetArquivoMagnetico;
    property Registro1X: IRegistro1X read GetRegistro1X write SetRegistro1X;
    property Registro5X: IRegistro5XLista read GetRegistro5X write SetRegistro5X;
    property Registro60M: IRegistro60MList read GetRegistro60M write SetRegistro60M;
    property Registro60R: IRegistro60RLista read GetRegistro60R write SetRegistro60R;
    property Registro61: IRegistro61Lista read GetRegistro61 write SetRegistro61;
    property Registro61R: IRegistro61RLista read GetRegistro61R write SetRegistro61R;
    property Registro55: IRegistro55Lista read GetRegistro55 write SetRegistro55;
    property Registro75: IRegistro75Lista read GetRegistro75 write SetRegistro75;
    property Registro74: IRegistro74Lista read GetRegistro74 write SetRegistro74;
  end;

implementation

end.
