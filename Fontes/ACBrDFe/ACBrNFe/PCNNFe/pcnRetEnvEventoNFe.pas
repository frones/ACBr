////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcnRetEnvEventoNFe;

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IfEnd}
  pcnConversao, pcnLeitor, pcnEventoNFe, pcnSignature,
  ACBrBase,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML;

type
  TRetInfEventoCollectionItem = class;
  TRetEventoNFe               = class;

  TRetInfEventoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TRetInfEventoCollectionItem;
    procedure SetItem(Index: Integer; Value: TRetInfEventoCollectionItem);
  public
    function Add: TRetInfEventoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TRetInfEventoCollectionItem;
    property Items[Index: Integer]: TRetInfEventoCollectionItem read GetItem write SetItem; default;
  end;

  TRetInfEventoCollectionItem = class(TObject)
  private
    FRetInfEvento: TRetInfEvento;
  public
    constructor Create;
    destructor Destroy; override;
    property RetInfEvento: TRetInfEvento read FRetInfEvento write FRetInfEvento;
  end;

  TRetEventoNFe = class(TObject)
  private
    FidLote: Int64;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FLeitor: TLeitor;
    FcStat: Integer;
    FcOrgao: Integer;
    FxMotivo: String;
    FretEvento: TRetInfEventoCollection;
    FInfEvento: TInfEvento;
    Fsignature: Tsignature;
    FXML: AnsiString;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;

    property idLote: Int64                      read FidLote    write FidLote;
    property Leitor: TLeitor                    read FLeitor    write FLeitor;
    property versao: String                     read Fversao    write Fversao;
    property tpAmb: TpcnTipoAmbiente            read FtpAmb     write FtpAmb;
    property verAplic: String                   read FverAplic  write FverAplic;
    property cOrgao: Integer                    read FcOrgao    write FcOrgao;
    property cStat: Integer                     read FcStat     write FcStat;
    property xMotivo: String                    read FxMotivo   write FxMotivo;
    property InfEvento: TInfEvento              read FInfEvento write FInfEvento;
    property signature: Tsignature              read Fsignature write Fsignature;
    property retEvento: TRetInfEventoCollection read FretEvento write FretEvento;
    property XML: AnsiString                    read FXML       write FXML;
  end;


implementation

uses
  pcnConversaoNFe;

{ TRetInfEventoCollection }

function TRetInfEventoCollection.Add: TRetInfEventoCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfEventoCollection.GetItem(Index: Integer): TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem(inherited Items[Index]);
end;

procedure TRetInfEventoCollection.SetItem(Index: Integer;
  Value: TRetInfEventoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TRetInfEventoCollection.New: TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetInfEventoCollectionItem }

constructor TRetInfEventoCollectionItem.Create;
begin
  inherited Create;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TRetInfEventoCollectionItem.Destroy;
begin
  FRetInfEvento.Free;
  inherited;
end;

{ TRetEventoNFe }

constructor TRetEventoNFe.Create;
begin
  inherited Create;
  FLeitor := TLeitor.Create;
  FretEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoNFe.Destroy;
begin
  FLeitor.Free;
  FretEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;
  inherited;
end;

function TRetEventoNFe.LerXml: Boolean;
var
  ok: Boolean;
  i, j: Integer;
begin
  Result := False;
  i:=0;
  try
    if (Leitor.rExtrai(1, 'evento') <> '') then
    begin
      if Leitor.rExtrai(2, 'infEvento', '', i + 1) <> '' then
      begin
        infEvento.ID           := Leitor.rAtributo('Id');
        InfEvento.cOrgao       := Leitor.rCampo(tcInt, 'cOrgao');
        infEvento.tpAmb        := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
        infEvento.CNPJ         := Leitor.rCampo(tcStr, 'CNPJ');

        if InfEvento.CNPJ = EmptyStr then
           infEvento.CNPJ := Leitor.rCampo(tcStr, 'CPF');

        infEvento.chNFe        := Leitor.rCampo(tcStr, 'chNFe');
        infEvento.dhEvento     := Leitor.rCampo(tcDatHor, 'dhEvento');
        infEvento.tpEvento     := StrToTpEventoNFe(ok,Leitor.rCampo(tcStr, 'tpEvento'));
        infEvento.nSeqEvento   := Leitor.rCampo(tcInt, 'nSeqEvento');
        infEvento.VersaoEvento := Leitor.rCampo(tcDe2, 'verEvento');

        if Leitor.rExtrai(3, 'detEvento', '', i + 1) <> '' then
        begin
          infEvento.DetEvento.descEvento := Leitor.rCampo(tcStr, 'descEvento');
          infEvento.DetEvento.xCorrecao  := Leitor.rCampo(tcStr, 'xCorrecao');
          infEvento.DetEvento.xCondUso   := Leitor.rCampo(tcStr, 'xCondUso');
          infEvento.DetEvento.nProt      := Leitor.rCampo(tcStr, 'nProt');
          infEvento.DetEvento.xJust      := Leitor.rCampo(tcStr, 'xJust');

          InfEvento.detEvento.cOrgaoAutor := Leitor.rCampo(tcInt, 'cOrgaoAutor');
          infEvento.detEvento.tpAutor     := StrToTipoAutor(ok, Leitor.rCampo(tcStr, 'tpAutor'));
          infEvento.detEvento.verAplic    := Leitor.rCampo(tcStr, 'verAplic');
          infEvento.detEvento.dhEmi       := Leitor.rCampo(tcDatHor, 'dhEmi');
          infEvento.detEvento.tpNF        := StrToTpNF(ok, Leitor.rCampo(tcStr, 'tpNF'));
          infEvento.detEvento.IE          := Leitor.rCampo(tcStr, 'IE');

          // Comprovante de Entrega da NF-e e o Cancelamento do Comprovante
          infEvento.detEvento.dhEntrega := Leitor.rCampo(tcDatHor, 'dhEntrega');
          infEvento.detEvento.nDoc      := Leitor.rCampo(tcStr, 'nDoc');
          infEvento.detEvento.xNome     := Leitor.rCampo(tcStr, 'xNome');
          infEvento.detEvento.latGPS    := Leitor.rCampo(tcDe6, 'latGPS');
          infEvento.detEvento.longGPS   := Leitor.rCampo(tcDe6, 'longGPS');

          infEvento.detEvento.hashComprovante   := Leitor.rCampo(tcStr, 'hashComprovante');
          infEvento.detEvento.dhHashComprovante := Leitor.rCampo(tcDatHor, 'dhHashComprovante');

          infEvento.detEvento.nProtEvento := Leitor.rCampo(tcStr, 'nProtEvento');

          infEvento.detEvento.tpAutorizacao := StrToAutorizacao(ok, Leitor.rCampo(tcStr, 'tpAutorizacao'));

          infEvento.detEvento.dhTentativaEntrega := Leitor.rCampo(tcDatHor, 'dhTentativaEntrega');
          infEvento.detEvento.nTentativa := Leitor.rCampo(tcInt, 'nTentativa');
          infEvento.detEvento.tpMotivo := StrTotpMotivo(ok, Leitor.rCampo(tcStr, 'tpMotivo'));
          infEvento.detEvento.xJustMotivo := Leitor.rCampo(tcStr, 'xJustMotivo');
          infEvento.detEvento.hashTentativaEntrega := Leitor.rCampo(tcStr, 'hashTentativaEntrega');
          infEvento.detEvento.dhHashTentativaEntrega := Leitor.rCampo(tcDatHor, 'dhHashTentativaEntrega');

//           infEvento.detEvento.vNF         := Leitor.rCampo(tcDe2, 'vNF');
//           infEvento.detEvento.vICMS       := Leitor.rCampo(tcDe2, 'vICMS');
//           infEvento.detEvento.vST         := Leitor.rCampo(tcDe2, 'vST');

          if Leitor.rExtrai(4, 'dest', '', i + 1) <> '' then
          begin
            infEvento.detEvento.dest.UF            := Leitor.rCampo(tcStr, 'UF');
            infEvento.detEvento.dest.CNPJCPF       := Leitor.rCampoCNPJCPF;
            infEvento.detEvento.dest.idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');
            infEvento.detEvento.dest.IE            := Leitor.rCampo(tcStr, 'IE');

            // para ficar em conformidade com o Schema
            infEvento.detEvento.vNF   := Leitor.rCampo(tcDe2, 'vNF');
            infEvento.detEvento.vICMS := Leitor.rCampo(tcDe2, 'vICMS');
            infEvento.detEvento.vST   := Leitor.rCampo(tcDe2, 'vST');
          end;

          i := 0;
          while Leitor.rExtrai(4, 'autXML', '', i + 1) <> '' do
          begin
            InfEvento.detEvento.autXML.New;

            InfEvento.detEvento.autXML.Items[i].CNPJCPF := Leitor.rCampoCNPJCPF;

            inc(i);
          end;
        end;
      end;

      if Leitor.rExtrai(2, 'Signature', '', i + 1) <> '' then
      begin
        signature.URI             := Leitor.rAtributo('Reference URI=');
        signature.DigestValue     := Leitor.rCampo(tcStr, 'DigestValue');
        signature.SignatureValue  := Leitor.rCampo(tcStr, 'SignatureValue');
        signature.X509Certificate := Leitor.rCampo(tcStr, 'X509Certificate');
      end;

      Result := True;
    end;

    if (Leitor.rExtrai(1, 'retEnvEvento') <> '') or
       (Leitor.rExtrai(1, 'retEvento') <> '') then
    begin
               Fversao   := Leitor.rAtributo('versao');
      (*HR03 *)FidLote   := Leitor.rCampo(tcInt64, 'idLote');
      (*HR04 *)FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      (*HR05 *)FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      (*HR06 *)FcOrgao   := Leitor.rCampo(tcInt, 'cOrgao');
      (*HR07 *)FcStat    := Leitor.rCampo(tcInt, 'cStat');
      (*HR08 *)FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      i := 0;
      while Leitor.rExtrai(2, 'infEvento', '', i + 1) <> '' do
      begin
        FretEvento.New;

        // Incluido por Italo em 07/05/2014
        FretEvento.Items[i].FRetInfEvento.XML := ConverteXMLtoUTF8(Leitor.Grupo);

//        (*HR10 *)FretEvento.versao               := Leitor.rCampo(tcStr, 'versao');
        (*HR12 *)FretEvento.Items[i].FRetInfEvento.Id         := Leitor.rAtributo('Id');
        (*HR13 *)FretEvento.Items[i].FRetInfEvento.tpAmb      := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
        (*HR14 *)FretEvento.Items[i].FRetInfEvento.verAplic   := Leitor.rCampo(tcStr, 'verAplic');
        (*HR15 *)FretEvento.Items[i].FRetInfEvento.cOrgao     := Leitor.rCampo(tcInt, 'cOrgao');
        (*HR16 *)FretEvento.Items[i].FRetInfEvento.cStat      := Leitor.rCampo(tcInt, 'cStat');
        (*HR17 *)FretEvento.Items[i].FRetInfEvento.xMotivo    := Leitor.rCampo(tcStr, 'xMotivo');
        (*HR18 *)FretEvento.Items[i].FRetInfEvento.chNFe      := Leitor.rCampo(tcStr, 'chNFe');
        (*HR19 *)FretEvento.Items[i].FRetInfEvento.tpEvento   := StrToTpEventoNFe(ok,Leitor.rCampo(tcStr, 'tpEvento'));
        (*HR20 *)FretEvento.Items[i].FRetInfEvento.xEvento    := Leitor.rCampo(tcStr, 'xEvento');
        (*HR21 *)FretEvento.Items[i].FRetInfEvento.nSeqEvento := Leitor.rCampo(tcInt, 'nSeqEvento');
        (*HR22 *)FretEvento.Items[i].FRetInfEvento.CNPJDest   := Leitor.rCampo(tcStr, 'CNPJDest');

        if FretEvento.Items[i].FRetInfEvento.CNPJDest = '' then
          (*HR23 *)FretEvento.Items[i].FRetInfEvento.CNPJDest  := Leitor.rCampo(tcStr, 'CPFDest');

        (*HR24 *)FretEvento.Items[i].FRetInfEvento.emailDest   := Leitor.rCampo(tcStr, 'emailDest');
                 FretEvento.Items[i].FRetInfEvento.cOrgaoAutor := Leitor.rCampo(tcInt, 'cOrgaoAutor');
        (*HR25 *)FretEvento.Items[i].FRetInfEvento.dhRegEvento := Leitor.rCampo(tcDatHor, 'dhRegEvento');
        (*HR26 *)FretEvento.Items[i].FRetInfEvento.nProt       := Leitor.rCampo(tcStr, 'nProt');
//                FretEvento.Items[i].FRetInfEvento.chNFePend   := Leitor.rCampo(tcStr, 'chNFePend');

        j := 0;
        while  Leitor.rExtrai(3, 'chNFePend', '', j + 1) <> '' do
        begin
          FretEvento.Items[i].FRetInfEvento.chNFePend.New;

          FretEvento.Items[i].FRetInfEvento.chNFePend[j].ChavePend := Leitor.rCampo(tcStr, 'chNFePend');

          inc(j);
        end;

        inc(i);
      end;

      Result := True;
    end;
  except
    result := False;
  end;
end;

end.
