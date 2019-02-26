////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar CTe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da CTe          //
//                                                                            //
//        site: www.projetocooperar.org/CTe                                   //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_CTe/        //
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
//              "PCN  -  Projeto  Cooperar  CTe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{$I ACBr.inc}

unit pcteRetInutCTe;

interface

uses
  SysUtils, Classes, pcnAuxiliar, pcnConversao, pcnLeitor;

type

  TRetInutCTe = class(TObject)
  private
    FLeitor: TLeitor;
    Fversao: String;
    FId: String;
    FtpAmb: TpcnTipoAmbiente;
    FverAplic: String;
    FcStat: Integer;
    FxMotivo: String;
    FxJust: String;
    FcUF: Integer;
    Fano: Integer;
    FCNPJ: String;
    FModelo: Integer;
    FSerie: Integer;
    FnCTIni: Integer;
    FnCTFin: Integer;
    FdhRecbto: TDateTime;
    FnProt: String;
  public
    constructor Create;
    destructor Destroy; override;
    function LerXml: boolean;
    property Leitor: TLeitor         read FLeitor   write FLeitor;
    property versao: String          read Fversao   write Fversao;
    property Id: String              read FId       write FId;
    property tpAmb: TpcnTipoAmbiente read FtpAmb    write FtpAmb;
    property verAplic: String        read FverAplic write FverAplic;
    property cStat: Integer          read FcStat    write FcStat;
    property xMotivo: String         read FxMotivo  write FxMotivo;
    property xJust: String           read FxJust    write FxJust;
    property cUF: Integer            read FcUF      write FcUF;
    property ano: Integer            read Fano      write Fano;
    property CNPJ: String            read FCNPJ     write FCNPJ;
    property Modelo: Integer         read FModelo   write FModelo;
    property Serie: Integer          read FSerie    write FSerie;
    property nCTIni: Integer         read FnCTIni   write FnCTIni;
    property nCTFin: Integer         read FnCTFin   write FnCTFin;
    property dhRecbto: TDateTime     read FdhRecbto write FdhRecbto;
    property nProt: String           read FnProt    write FnProt;
  end;

implementation

{ TretAtuCadEmiDFe }

constructor TRetInutCTe.Create;
begin
  inherited Create;
  FLeitor := TLeitor.Create;
end;

destructor TRetInutCTe.Destroy;
begin
  FLeitor.Free;
  inherited;
end;

function TRetInutCTe.LerXml: boolean;
var
  ok: boolean;
begin
  Result := False;
  try
    if (leitor.rExtrai(1, 'inutCTe') <> '') then
      FId := Leitor.rAtributo('Id=');

    if (leitor.rExtrai(1, 'retInutCTe') <> '') or (leitor.rExtrai(1, 'infInut') <> '') then
    begin
               Fversao   := Leitor.rAtributo('versao', 'retInutCTe');
      (*DR05 *)FtpAmb    := StrToTpAmb(ok, Leitor.rCampo(tcStr, 'tpAmb'));
      (*DR06 *)FverAplic := Leitor.rCampo(tcStr, 'verAplic');
      (*DR07 *)FcStat    := Leitor.rCampo(tcInt, 'cStat');
      (*DR08 *)FxMotivo  := Leitor.rCampo(tcStr, 'xMotivo');
      (*DR09 *)FcUF      := Leitor.rCampo(tcInt, 'cUF');

      if cStat = 102 then
      begin
        (*DR10 *)Fano      := Leitor.rCampo(tcInt, 'ano');
        (*DR11 *)FCNPJ     := Leitor.rCampo(tcStr, 'CNPJ');
        (*DR12 *)FModelo   := Leitor.rCampo(tcInt, 'mod');
        (*DR13 *)FSerie    := Leitor.rCampo(tcInt, 'serie');
        (*DR14 *)FnCTIni   := Leitor.rCampo(tcInt, 'nCTIni');
        (*DR15 *)FnCTFin   := Leitor.rCampo(tcInt, 'nCTFin');
        (*DR16 *)FdhRecbto := Leitor.rCampo(tcDatHor, 'dhRecbto');
        (*DR17 *)FnProt    := Leitor.rCampo(tcStr, 'nProt');
      end;

      if leitor.rExtrai(1, 'infInut') <> '' then
        FxJust := Leitor.rCampo(tcStr, 'xJust');

      Result := True;
    end;
  except
    result := False;
  end;
end;

end.

