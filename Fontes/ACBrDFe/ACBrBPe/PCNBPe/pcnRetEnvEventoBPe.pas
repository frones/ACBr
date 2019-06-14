////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar BPe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml do BPe          //
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

{*******************************************************************************
|* Historico
|*
|* 20/06/2017: Italo Jurisato Junior
|*  - Doação do componente para o Projeto ACBr
*******************************************************************************}

{$I ACBr.inc}

unit pcnRetEnvEventoBPe;

interface

uses
  SysUtils, Classes, Contnrs,
  pcnConversao, pcnLeitor, pcnEventoBPe, pcnSignature;

type
  TRetInfEventoCollectionItem = class;

  TRetInfEventoCollection = class(TObjectList)
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

  TRetEventoBPe = class(TObject)
  private
    FidLote: Integer;
    Fversao: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FLeitor: TLeitor;
    FcStat: Integer;
    FcOrgao: Integer;
    FxMotivo: String;
    FretEvento: TRetInfEventoCollection;
    FInfEvento: TInfEvento;
    FXML: AnsiString;
    Fsignature: Tsignature;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: Boolean;
    property idLote: Integer                    read FidLote    write FidLote;
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
  pcnConversaoBPe;

{ TRetInfEventoCollection }

function TRetInfEventoCollection.Add: TRetInfEventoCollectionItem;
begin
  Result := Self.New;
end;

function TRetInfEventoCollection.GetItem(
  Index: Integer): TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem(inherited GetItem(Index));
end;

procedure TRetInfEventoCollection.SetItem(Index: Integer;
  Value: TRetInfEventoCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TRetInfEventoCollection.New: TRetInfEventoCollectionItem;
begin
  Result := TRetInfEventoCollectionItem.Create;
  Self.Add(Result);
end;

{ TRetInfEventoCollectionItem }

constructor TRetInfEventoCollectionItem.Create;
begin
  inherited;
  FRetInfEvento := TRetInfEvento.Create;
end;

destructor TRetInfEventoCollectionItem.Destroy;
begin
  FRetInfEvento.Free;
  inherited;
end;

{ TRetEventoBPe }

constructor TRetEventoBPe.Create;
begin
  inherited;
  FLeitor    := TLeitor.Create;
  FretEvento := TRetInfEventoCollection.Create;
  FInfEvento := TInfEvento.Create;
  Fsignature := Tsignature.Create;
end;

destructor TRetEventoBPe.Destroy;
begin
  FLeitor.Free;
  FretEvento.Free;
  FInfEvento.Free;
  Fsignature.Free;
  inherited;
end;

function TRetEventoBPe.LerXml: Boolean;
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
         infEvento.chBPe        := Leitor.rCampo(tcStr, 'chBPe');
         infEvento.dhEvento     := Leitor.rCampo(tcDatHor, 'dhEvento');
         infEvento.tpEvento     := StrToTpEventoBPe(ok, Leitor.rCampo(tcStr, 'tpEvento'));
         infEvento.nSeqEvento   := Leitor.rCampo(tcInt, 'nSeqEvento');
         infEvento.VersaoEvento := Leitor.rCampo(tcDe2, 'verEvento');

         if Leitor.rExtrai(3, 'detEvento', '', i + 1) <> '' then
         begin
           infEvento.DetEvento.xCorrecao := Leitor.rCampo(tcStr, 'xCorrecao');
           infEvento.DetEvento.xCondUso  := Leitor.rCampo(tcStr, 'xCondUso');
           infEvento.DetEvento.nProt     := Leitor.rCampo(tcStr, 'nProt');
           infEvento.DetEvento.xJust     := Leitor.rCampo(tcStr, 'xJust');
           infEvento.DetEvento.poltrona  := Leitor.rCampo(tcInt, 'poltrona');

           InfEvento.detEvento.cOrgaoAutor := Leitor.rCampo(tcInt, 'cOrgaoAutor');
           infEvento.detEvento.tpAutor     := StrToTipoAutor(ok, Leitor.rCampo(tcStr, 'tpAutor'));
           infEvento.detEvento.verAplic    := Leitor.rCampo(tcStr, 'verAplic');
           infEvento.detEvento.dhEmi       := Leitor.rCampo(tcDatHor, 'dhEmi');
           infEvento.detEvento.tpBPe       := StrToTpBPe(ok, Leitor.rCampo(tcStr, 'tpBPe'));
           infEvento.detEvento.IE          := Leitor.rCampo(tcStr, 'IE');

           if Leitor.rExtrai(4, 'dest', '', i + 1) <> '' then
           begin
             infEvento.detEvento.dest.UF            := Leitor.rCampo(tcStr, 'UF');
             infEvento.detEvento.dest.CNPJCPF       := Leitor.rCampoCNPJCPF;
             infEvento.detEvento.dest.idEstrangeiro := Leitor.rCampo(tcStr, 'idEstrangeiro');
             infEvento.detEvento.dest.IE            := Leitor.rCampo(tcStr, 'IE');

             infEvento.detEvento.vNF   := Leitor.rCampo(tcDe2, 'vNF');
             infEvento.detEvento.vICMS := Leitor.rCampo(tcDe2, 'vICMS');
             infEvento.detEvento.vST   := Leitor.rCampo(tcDe2, 'vST');
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
       (Leitor.rExtrai(1, 'retEventoBPe') <> '') then
    begin
      Fversao   := Leitor.rAtributo('versao');
      FidLote   := Leitor.rCampo(tcInt, 'idLote');
      FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      FcOrgao   := Leitor.rCampo(tcInt, 'cOrgao');
      FcStat    := Leitor.rCampo(tcInt, 'cStat');
      FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');

      i := 0;
      while Leitor.rExtrai(2, 'infEvento', '', i + 1) <> '' do
       begin
         FretEvento.New;

         FretEvento.Items[i].FRetInfEvento.XML := Leitor.Grupo;

         FretEvento.Items[i].FRetInfEvento.Id         := Leitor.rAtributo('Id');
         FretEvento.Items[i].FRetInfEvento.tpAmb      := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
         FretEvento.Items[i].FRetInfEvento.verAplic   := Leitor.rCampo(tcStr, 'verAplic');
         FretEvento.Items[i].FRetInfEvento.cOrgao     := Leitor.rCampo(tcInt, 'cOrgao');
         FretEvento.Items[i].FRetInfEvento.cStat      := Leitor.rCampo(tcInt, 'cStat');
         FretEvento.Items[i].FRetInfEvento.xMotivo    := Leitor.rCampo(tcStr, 'xMotivo');
         
         FretEvento.Items[i].FRetInfEvento.chBPe      := Leitor.rCampo(tcStr, 'chBPe');
         // Alterado a função de conversao
         FretEvento.Items[i].FRetInfEvento.tpEvento   := StrToTpEventoBPe(ok, Leitor.rCampo(tcStr, 'tpEvento'));
         FretEvento.Items[i].FRetInfEvento.xEvento    := Leitor.rCampo(tcStr, 'xEvento');
         FretEvento.Items[i].FRetInfEvento.nSeqEvento := Leitor.rCampo(tcInt, 'nSeqEvento');
         FretEvento.Items[i].FRetInfEvento.dhRegEvento := Leitor.rCampo(tcDatHor, 'dhRegEvento');
         FretEvento.Items[i].FRetInfEvento.nProt       := Leitor.rCampo(tcStr, 'nProt');

         FretEvento.Items[i].FRetInfEvento.CNPJDest   := Leitor.rCampo(tcStr, 'CNPJDest');

         if FretEvento.Items[i].FRetInfEvento.CNPJDest = '' then
           FretEvento.Items[i].FRetInfEvento.CNPJDest  := Leitor.rCampo(tcStr, 'CPFDest');

         FretEvento.Items[i].FRetInfEvento.emailDest   := Leitor.rCampo(tcStr, 'emailDest');
         FretEvento.Items[i].FRetInfEvento.cOrgaoAutor := Leitor.rCampo(tcInt, 'cOrgaoAutor');

         j := 0;
         while  Leitor.rExtrai(3, 'chBPePend', '', j + 1) <> '' do
          begin
            FretEvento.Items[i].FRetInfEvento.chBPePend.New;

            FretEvento.Items[i].FRetInfEvento.chBPePend[j].ChavePend := Leitor.rCampo(tcStr, 'chBPePend');

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
