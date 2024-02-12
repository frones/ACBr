{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ACBrDFeComum.RetConsCad;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  ACBrBase, ACBrXmlBase;

type

  TInfCadCollectionItem = class(TObject)
  private
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FUF: string;
    FcSit: Integer;
    FindCredNFe: Integer;
    FindCredCTe: Integer;
    FxNome: string;
    FxFant: string;
    FxRegApur: string;
    FCNAE: Integer;
    FdIniAtiv: TDateTime;
    FdUltSit: TDateTime;
    FdBaixa: TDateTime;
    FIEUnica: string;
    FIEAtual: string;
    FxLgr: string;
    Fnro: string;
    FxCpl: string;
    FxBairro: string;
    FcMun: Integer;
    FxMun: string;
    FCep: Integer;
  public
    property IE: string          read FIE         write FIE;
    property CNPJ: string        read FCNPJ       write FCNPJ;
    property CPF: string         read FCPF        write FCPF;
    property UF: string          read FUF         write FUF;
    property cSit: Integer       read FcSit       write FcSit;
    property indCredNFe: Integer read FindCredNFe write FindCredNFe;
    property indCredCTe: Integer read FindCredCTe write FindCredCTe;
    property xNome: string       read FxNome      write FxNome;
    property xFant: string       read FxFant      write FxFant;
    property xRegApur: string    read FxRegApur   write FxRegApur;
    property CNAE: Integer       read FCNAE       write FCNAE;
    property dIniAtiv: TDateTime read FdIniAtiv   write FdIniAtiv;
    property dUltSit: TDateTime  read FdUltSit    write FdUltSit;
    property dBaixa: TDateTime   read FdBaixa     write FdBaixa;
    property IEUnica: string     read FIEUnica    write FIEUnica;
    property IEAtual: string     read FIEAtual    write FIEAtual;
    property xLgr: string        read FxLgr       write FxLgr;
    property nro: string         read Fnro        write Fnro;
    property xCpl: string        read FxCpl       write FxCpl;
    property xBairro: string     read FxBairro    write FxBairro;
    property cMun: Integer       read FcMun       write FcMun;
    property xMun: string        read FxMun       write FxMun;
    property CEP: Integer        read FCep        write FCep;
  end;

  TInfCadCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TInfCadCollectionItem;
    procedure SetItem(Index: Integer; Value: TInfCadCollectionItem);
  public
    function New: TInfCadCollectionItem;
    property Items[Index: Integer]: TInfCadCollectionItem read GetItem write SetItem; default;
  end;

  TRetConsCad = class(TObject)
  private
    Fversao: string;
    FverAplic: string;
    FcStat: Integer;
    FxMotivo: string;
    FUF: string;
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FdhCons: TDateTime;
    FcUF: Integer;
    FInfCad: TInfCadCollection;
    FXmlRetorno: string;

    procedure SetInfCad(const Value: TInfCadCollection);
  public
    constructor Create;
    destructor Destroy; override;

    function LerXML: Boolean;

    property versao: string            read Fversao   write Fversao;
    property verAplic: string          read FverAplic write FverAplic;
    property cStat: Integer            read FcStat    write FcStat;
    property xMotivo: string           read FxMotivo  write FxMotivo;
    property UF: string                read FUF       write FUF;
    property IE: string                read FIE       write FIE;
    property CNPJ: string              read FCNPJ     write FCNPJ;
    property CPF: string               read FCPF      write FCPF;
    property dhCons: TDateTime         read FdhCons   write FdhCons;
    property cUF: Integer              read FcUF      write FcUF;
    property InfCad: TInfCadCollection read FInfCad   write SetInfCad;

    property XmlRetorno: string read FXmlRetorno write FXmlRetorno;
  end;

implementation

uses
  ACBrUtil.Strings,
  ACBrXmlDocument;

{ TInfCadCollection }

function TInfCadCollection.GetItem(Index: Integer): TInfCadCollectionItem;
begin
  Result := TInfCadCollectionItem(inherited Items[Index]);
end;

procedure TInfCadCollection.SetItem(Index: Integer; Value: TInfCadCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TInfCadCollection.New: TInfCadCollectionItem;
begin
  Result := TInfCadCollectionItem.Create;
  Add(Result);
end;

{ RetConsCad }

constructor TRetConsCad.Create;
begin
  inherited Create;

  FInfCad := TInfCadCollection.Create();
end;

destructor TRetConsCad.Destroy;
begin
  FInfCad.Free;

  inherited;
end;

procedure TRetConsCad.SetInfCad(const Value: TInfCadCollection);
begin
  FInfCad.Assign(Value);
end;

function TRetConsCad.LerXML: Boolean;
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodes: TACBrXmlNodeArray;
  ok: Boolean;
  i: Integer;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Document.LoadFromXml(XmlRetorno);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        versao := ObterConteudoTag(ANode.Attributes.Items['versao']);

        AuxNode := ANode.Childrens.FindAnyNs('infCons');

        if AuxNode <> nil then
        begin
          // tratamento para quando o webservice não retorna os zeros a esquerda
          // na consulta
          CNPJ := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);

          if (CNPJ <> '') and (length(CNPJ) < 14) then
            CNPJ := PadLeft(CNPJ, 14, '0');

          CPF := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CPF'), tcStr);

          if (CPF <> '') and (length(CPF) < 11) then
            CPF := PadLeft(CPF, 11, '0');

          verAplic := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('verAplic'), tcStr);
          cStat := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cStat'), tcInt);
          xMotivo := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xMotivo'), tcStr));
          UF := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('UF'), tcStr));
          IE := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('IE'), tcStr));
          dhCons := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dhCons'), tcDatHor);
          cUF := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cUF'), tcInt);

          ANodes := AuxNode.Childrens.FindAllAnyNs('infCad');

          InfCad.Clear;

          for i := 0 to Length(ANodes) - 1 do
          begin
            InfCad.New;
            with InfCad[i] do
            begin
              // tratamento para quando o webservice não retorna os zeros a esquerda
              // na consulta
              CNPJ := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('CNPJ'), tcStr);

              if (CNPJ <> '') and (length(CNPJ) < 14) then
                CNPJ := PadLeft(CNPJ, 14, '0');

              CPF := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('CPF'), tcStr);

              if (CPF <> '') and (length(CPF) < 11) then
                CPF := PadLeft(CPF, 11, '0');

              IE := ACBrStr(ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('IE'), tcStr));
              UF := ACBrStr(ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('UF'), tcStr));
              cSit := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('cSit'), tcInt);
              indCredNFe := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('indCredNFe'), tcInt);
              indCredCTe := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('indCredCTe'), tcInt);
              xNome := ACBrStr(ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('xNome'), tcStr));
              xFant := ACBrStr(ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('xFant'), tcStr));
              xRegApur := ACBrStr(ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('xRegApur'), tcStr));
              CNAE := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('CNAE'), tcInt);
              dIniAtiv := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('dIniAtiv'), tcDat);
              dUltSit := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('dUltSit'), tcDat);
              dBaixa := ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('dBaixa'), tcDat);
              IEUnica := ACBrStr(ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('IEUnica'), tcStr));
              IEAtual := ACBrStr(ObterConteudoTag(ANodes[i].Childrens.FindAnyNs('IEAtual'), tcStr));

              AuxNode := ANodes[i].Childrens.FindAnyNs('ender');

              if AuxNode <> nil then
              begin
                xLgr := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xLgr'), tcStr));
                nro := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('nro'), tcStr));
                xCpl := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xCpl'), tcStr));
                xBairro := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xBairro'), tcStr));
                cMun := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('cMun'), tcInt);
                xMun := ACBrStr(ObterConteudoTag(AuxNode.Childrens.FindAnyNs('xMun'), tcStr));
                CEP := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CEP'), tcInt);
              end;
            end;
          end;
        end;
      end;

      Result := True;
    except
      Result := False;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

end.

