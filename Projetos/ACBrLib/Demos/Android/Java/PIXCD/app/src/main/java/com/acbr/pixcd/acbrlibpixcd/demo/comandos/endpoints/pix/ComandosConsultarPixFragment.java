package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.pix;

import android.os.Bundle;

import androidx.fragment.app.Fragment;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.EditText;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosConsultarPixFragment extends Fragment {

    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    private EditText txte2eidConsultarPIX;
    private Button btnConsultarPix;
    private EditText txtRespostaConsultarPIX;
    private Button btnLimparRespostaConsultarPIX;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_comandos_consultar_pix, container, false);

        ACBrPIXCD = ACBrLibHelper.getInstance("");

        txte2eidConsultarPIX = view.findViewById(R.id.txte2eidConsultarPIX);
        btnConsultarPix = view.findViewById(R.id.btnConsultarPix);
        txtRespostaConsultarPIX = view.findViewById(R.id.txtRespostaConsultarPIX);
        btnLimparRespostaConsultarPIX = view.findViewById(R.id.btnLimparRespostaConsultarPIX);

        application = (PIXCDApplication) this.getContext().getApplicationContext();

        btnConsultarPix.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                consultarPix();
            }
        });

        btnLimparRespostaConsultarPIX.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                LimparRespostaConsultarPix();
            }
        });

        return view;
    }

    public void consultarPix(){
        txtRespostaConsultarPIX.setText("");
        String result = "";
        String e2eid = txte2eidConsultarPIX.getText().toString();
        try{
            result = ACBrPIXCD.ConsultarPix(e2eid);
        } catch (Exception ex){
            Log.e("Erro ao ConsultaPix", ex.getMessage());
            result = ex.getMessage();
        } finally {
            txtRespostaConsultarPIX.setText(result);
        }
    }

    public void LimparRespostaConsultarPix(){
        txtRespostaConsultarPIX.setText("");
    }

}