<script>
    export let vline;
    </script>
    
    
    <g class="tooltip-lines" transform="translate(0, 0)">
        <!-- vertical line -->
        <line
          y1={vline.y1}
          y2={vline.y2}
          x1={vline.x1}
          x2={vline.x2}
        />
    </g>
    
    <style>
        .tooltip-lines line {
        stroke: #f1eeee;
        stroke-width: 2px;
        stroke-dasharray: 1.5px;
      }
    </style>