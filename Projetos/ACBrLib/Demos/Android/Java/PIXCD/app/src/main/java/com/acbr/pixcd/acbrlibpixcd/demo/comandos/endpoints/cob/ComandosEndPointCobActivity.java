package com.acbr.pixcd.acbrlibpixcd.demo.comandos.endpoints.cob;

import android.annotation.SuppressLint;
import android.os.Bundle;

import androidx.appcompat.app.AppCompatActivity;
import androidx.core.view.GravityCompat;

import com.acbr.pixcd.acbrlibpixcd.demo.R;
import com.acbr.pixcd.acbrlibpixcd.demo.databinding.ActivityComandosEndpointCobBinding;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ACBrLibHelper;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.PIXCDApplication;
import com.acbr.pixcd.acbrlibpixcd.demo.utils.ViewPagerAdapter;
import com.google.android.material.tabs.TabLayoutMediator;

import java.util.HashMap;
import java.util.Map;

import br.com.acbr.lib.pixcd.ACBrLibPIXCD;

public class ComandosEndPointCobActivity extends AppCompatActivity {

    private ActivityComandosEndpointCobBinding binding;
    private ACBrLibPIXCD ACBrPIXCD;
    private PIXCDApplication application;

    @SuppressLint("MissingInflatedId")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = ActivityComandosEndpointCobBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        configTabLayout();

        application = (PIXCDApplication) getApplicationContext();
        ACBrPIXCD = ACBrLibHelper.getInstance("");
    }

    private void configTabLayout(){
        ViewPagerAdapter adapter = new ViewPagerAdapter(this);
        binding.viewPager.setAdapter(adapter);

        adapter.addFragment(new ComandosCriarCobrancaImediataFragment(), "Criar Cobrança Imediata");
        adapter.addFragment(new ComandosConsultarCobrancaImediataFragment(), "Consultar Cobrança Imediata");
        adapter.addFragment(new ComandosConsultarCobrancasCobFragment(), "Consultar Cobranças Cob");
        adapter.addFragment(new ComandosRevisarCobrancaImediataFragment(), "Revisar Cobrança Imediata");
        adapter.addFragment(new ComandosCancelarCobrancaImediataFragment(), "Cancelar Cobrança Imediata");

        binding.viewPager.setOffscreenPageLimit(adapter.getItemCount());

        new TabLayoutMediator(binding.tabs, binding.viewPager, (tab, position) -> {
            tab.setText(adapter.getTitle(position));
        }).attach();

        Map<Integer, Integer> menuPositionMap = new HashMap<>();
        menuPositionMap.put(R.id.menu_criar_cobranca_imediata, 0);
        menuPositionMap.put(R.id.menu_consultar_cobranca_imediata, 1);
        menuPositionMap.put(R.id.menu_consultar_cobrancas_cob, 2);
        menuPositionMap.put(R.id.menu_revisar_cobranca_imediata, 3);
        menuPositionMap.put(R.id.menu_cancelar_cobranca_imediata, 4);

        binding.navigationView.setNavigationItemSelectedListener(item -> {
            Integer position = menuPositionMap.getOrDefault(item.getItemId(), 0);

            binding.viewPager.setCurrentItem(position, true);
            binding.drawerLayout.closeDrawer(GravityCompat.START);
            return true;
        });
    }
}